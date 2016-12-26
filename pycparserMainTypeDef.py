from __future__ import print_function

import os
from pycparser import c_parser, c_ast, parse_file
import sys, os

__version__ = "V2.0"
typeList = ["char", "short", "int", "long", "long long",
            "unsigned char", "unsigned short", "unsigned int",
            "unsigned long", "unsigned long long",
            "struct", "void", "unkown"]

tlvDataTypeDict = {
    "char": "TLV_TYPE_L8",
    "short": "TLV_TYPE_L16",
    "int": "TLV_TYPE_L32",
    "long": "TLV_TYPE_L64",
    "long long": "TLV_TYPE_L64",
    "unsigned char": "TLV_TYPE_L8",
    "unsigned short": "TLV_TYPE_L16",
    "unsigned int": "TLV_TYPE_L32",
    "unsigned long": "TLV_TYPE_L64",
    "unsigned long long": "TLV_TYPE_L64",
    "struct": "TLV_TYPE_STRUCT",
}


def _explain_type(decl, decl_name='', instance=False):
    # type: (object, object, object) -> object
    """ Recursively explains a type decl node
    return :  int a
            char b
            short c
            unsigned long d
            int * e
            int b[10]
            struct bitmask_t portmask[12]
    """
    typ = type(decl)
    #print("_explain_type:typ=%s" % typ)
    if typ == c_ast.TypeDecl:
        quals = ' '.join(decl.quals) + ' ' if decl.quals else ''
        #print("typ == c_ast.TypeDecl : " + quals)
        return quals + _explain_type(decl.type, instance=instance)

    elif typ == c_ast.Typedef:  # Typedef <ext[0]>: name=newbitmask_t, quals=[], storage=['Typedef']
        #print("_explain_type: type=%s decl.name=%s" % (typ, decl.name))
        return decl.name
        #   return _explain_type(decl.type, instance=instance)
        #   pass

    elif typ == c_ast.Typename or typ == c_ast.Decl:
        if type(decl.type) == c_ast.ArrayDecl:
            return _explain_type(decl.type, instance=instance)
        else:
            buf = _explain_type(decl.type, instance=instance) + decl_name
            #print ("typ == c_ast.Decl: %s" % buf)
            return buf
    elif typ == c_ast.IdentifierType:
        '''
            char, short, int, long, unsigned char, unsigned shor, unsigned int, unsigned long.
        '''
        #print("typ == c_ast.IdentifierType : " + ' '.join(decl.names))
        return ' '.join(decl.names) + decl_name
    elif typ == c_ast.PtrDecl:
        quals = ' '.join(decl.quals) + ' ' if decl.quals else ''
        if (instance == True):
            return quals + _explain_type(decl.type, instance=instance)
        else:
            return quals + _explain_type(decl.type, instance=instance) + ' *'
    elif typ == c_ast.ArrayDecl:
        arr = ''
        if decl.dim: arr += '%s[%s]' % (decl.type.declname, decl.dim.value)

        return _explain_type(decl.type, instance=instance) + '\t' + arr

    elif typ == c_ast.FuncDecl:
        '''
            Notes: not used
        '''
        if decl.args:
            params = [_explain_type(param, instance=instance) for param in decl.args.params]
            args = ', '.join(params)
        else:
            args = ''
        return ('function(%s) returning ' % (args) +
                _explain_type(decl.type, instance=instance))
    elif typ == c_ast.Struct:
        #decl.show()
        return "struct " + ''.join(decl.name) + decl_name

    else:
        print("No type matched for TYPE=%s!" % (typ))


def _baseTypeLenBytes(type, onlyDataLen=False):
    typeDic = {
        "char": 1,
        "short": 2,
        "int": 4,
        "long": 8,
        "unsigned char": 1,
        "unsigned short": 2,
        "unsigned int": 4,
        "unsigned long": 8
    }
    if typeDic.has_key(type):
        if onlyDataLen:
            return typeDic[type]
        else:
            return typeDic[type] + 4

    length = -1
    return length


def _elemTypeCodeArrFlagGet(ast_node):
    '''
        Return :
            0,0  - int, not array
            1,2  - short, array size if 2. eg. short x[2]
            10,0 - struct, not array.    eg. struct bitmask_t mask
    '''
    _dataType = 0
    _arraySize = 0

    decl = ast_node
    typ = type(decl)

    # print("1Typeof ast_node = %s" % typ)
    try:
        if typ == c_ast.Typename or typ == c_ast.Decl or \
                        c_ast.PtrDecl == typ or typ == c_ast.TypeDecl:
            return _elemTypeCodeArrFlagGet(decl.type)
        elif typ == c_ast.IdentifierType:
            '''
                Key: char, short, int, long, unsigned char, unsigned short, unsigned int, unsigned long.
            '''
            try:
                typeStr = ' '.join(decl.names)
                # print ("TypeStr = %s" % typeStr)
                if typeStr in typeList:
                    _dataType = typeList.index(typeStr)
                else:
                    print("TypeStr = %s" % typeStr)
                    return typeList.index("unkown")

            except:
                print("typeList.index(typeStr) failed for %s!" % (typeStr))
                _dataType = 100

            return _dataType, _arraySize
        elif typ == c_ast.ArrayDecl:
            _arraySize = int(decl.dim.value)
            (_dataType, _arraySizeSub) = _elemTypeCodeArrFlagGet(decl.type)
            return _dataType, _arraySize
        elif typ == c_ast.Struct:
            # ast_node.show(attrnames=True, nodenames=True, showcoord=False)
            _dataType = typeList.index("struct")
            return _dataType, _arraySize
    except:
        print("Data_type_get failed! ")


def _structNameGet(ast_node):
    """
        Return: return the name of struct ast_node.
    """
    decl = ast_node
    typ = type(decl)
    try:
        if typ == c_ast.Typename or typ == c_ast.Decl or \
                        c_ast.PtrDecl == typ or typ == c_ast.TypeDecl or typ == c_ast.ArrayDecl:
            return _structNameGet(decl.type)
        elif typ == c_ast.IdentifierType:
            return "Not struct"
        elif typ == c_ast.Struct:
            return decl.name
    except:
        print("_structNameGet failed! ")


class ElemIbcMsgLenParser(c_ast.NodeVisitor):
    """ Recursively explains a type decl node
    """

    def __init__(self, FileAST=None):
        self.FileAst = FileAST
        self.lookforStructNode = None
        self.lookforStructNodeName = ""
        self.lookforTypedefNode = None
        self.lookforTypedefNodeName = ""
        pass

    def visit_Struct(self, struct_node):
        try:
            if type(struct_node.decls) is type(None): return -1  # Avoid the AST reference struct node.
        except:
            print("visit_Struct  failed!")
        if struct_node.name == self.lookforStructNodeName:
            self.lookforStructNode = struct_node
        pass

    def visit_Typedef(self, typedef_node):
        try:
            if typedef_node.name == self.lookforTypedefNodeName:
                self.lookforTypedefNode = typedef_node
            pass
        except:
            print("visit_Typedef failed!")

    def FindStructNodeByName(self, name):
        try:
            self.lookforStructNode = None
            self.lookforStructNodeName = name
            self.visit(self.FileAst)
        except:
            print("Find Struct(%s) node failed! " % name)
        return self.lookforStructNode

    def FindTypedefNodeByName(self, name):
        try:
            self.lookforTypedefNode = None
            self.lookforTypedefNodeName = name
            self.visit(self.FileAst)
            pass
        except:
            print("Find Typedef(%s) node failed! " % name)

        return self.lookforTypedefNode

    def doParse(self, decl, onlyDataLen=False):
        '''Return: decl tlv encode length'''
        typ = type(decl)
        try:
            if typ == c_ast.Typedef or typ == c_ast.TypeDecl:
                return self.doParse(decl.type, onlyDataLen=onlyDataLen)
            elif typ == c_ast.Typename or typ == c_ast.Decl or c_ast.PtrDecl == typ:
                return self.doParse(decl.type, onlyDataLen=onlyDataLen)
            elif typ == c_ast.IdentifierType:
                '''
                    Key: char, short, int, long, unsigned char, unsigned short, unsigned int, unsigned long.
                '''
                Key = ' '.join(decl.names)
                #decl.show(attrnames=True, nodenames=True, showcoord=False)

                typeLen = _baseTypeLenBytes(Key, onlyDataLen=onlyDataLen)
                if typeLen == -1:
                    typedefNode = self.FindTypedefNodeByName(Key)
                    typeLen = self.doParse(typedefNode, onlyDataLen=onlyDataLen)
                #print("typeLen = %d" % typeLen)
                return typeLen
            elif typ == c_ast.ArrayDecl:
                BaseLen = 8
                ArrayElemSize = int(decl.dim.value)
                ArrayElemLen = self.doParse(decl.type, onlyDataLen=True)
                ArrayBuffLen = ArrayElemLen * ArrayElemSize + BaseLen
                # print("Elem size = %d, elem length = %d, data len=%d" % (ArrayElemSize, ArrayElemLen, ArrayBuffLen))
                return ArrayBuffLen
            elif typ == c_ast.Struct:
                BaseLen = 8
                DataLen = 0
                struct_node = decl
                if type(decl.decls) is type(None):
                    struct_node = self.FindStructNodeByName(decl.name)

                if struct_node is not None:
                    for (child_name, child) in struct_node.children():
                        DataLen += self.doParse(child, onlyDataLen=False)
                        # print("\t2child = %s, DataLen=%d " % (child.name, DataLen))

                return BaseLen + DataLen
            else:
                print("No type matched for TYPE=%s!" % (typ))
                return 0
        except:
            print("doParse failed! TYPE=%s!" % (typ))
        return 0

    def doParseTypeLenBytes(self, decl):
        '''Return the real data length of decl'''
        typ = type(decl)
        try:
            if typ == c_ast.Typedef or typ == c_ast.TypeDecl:
                return self.doParseTypeLenBytes(decl.type)
            elif typ == c_ast.Typename or typ == c_ast.Decl or c_ast.PtrDecl == typ:
                return self.doParseTypeLenBytes(decl.type)
            elif typ == c_ast.IdentifierType:
                '''
                    Key: char, short, int, long, unsigned char, unsigned short, unsigned int, unsigned long.
                '''
                Key = ' '.join(decl.names)

                typeLen = _baseTypeLenBytes(Key, onlyDataLen=True)
                if typeLen == -1:
                    typedefNode = self.FindTypedefNodeByName(Key)
                    typeLen = self.doParse(typedefNode)

                return typeLen
            elif typ == c_ast.ArrayDecl:
                BaseLen = 0
                # ArrayElemSize = int(decl.dim.value)    # Just type len!!!!!
                ArrayElemSize = 1
                ArrayElemLen = self.doParseTypeLenBytes(decl.type)
                ArrayBuffLen = ArrayElemLen * ArrayElemSize + BaseLen
                # print("Elem size = %d, elem length = %d, data len=%d" % (ArrayElemSize, ArrayElemLen, ArrayBuffLen))
                return ArrayBuffLen
            elif typ == c_ast.FuncDecl:
                return 8
                pass
            elif typ == c_ast.Struct:
                DataLen = 0
                struct_node = decl
                if type(decl.decls) is type(None):
                    struct_node = self.FindStructNodeByName(decl.name)

                if struct_node is not None:
                    for (child_name, child) in struct_node.children():
                        DataLen += self.doParseTypeLenBytes(child)
                        # print("\t2child = %s, DataLen=%d " % (child.name, DataLen))

                return DataLen
            else:
                print("No type matched for TYPE=%s!" % (typ))
                return 0
        except:
            print("doParseTypeLenBytes failed! TYPE=%s!" % (typ))
        return 0

    pass


class memberDecl(object):
    FileAST = None

    def __init__(self, ast_node, nodename, FileAST, debug=False):
        self.c_ast = FileAST
        self.ast_node = ast_node
        self.type = ""
        self.structname = ""
        self.typedefName = ""
        self.typeLenBytes = 0
        self.pointer = 0
        self.name = nodename
        self.arraySize = 0
        self.debug = debug
        self.IbcMsgLenParser = ElemIbcMsgLenParser(FileAST=FileAST)
        if memberDecl.FileAST is None:
            memberDecl.FileAST = FileAST

        try:
            self.parseNode(self.ast_node)
            self.parseNodeTypeLenBytes()
        except:
            print("Parse ast node failed!!!")

    def show(self):
        print("Ast node: %s\n\ttype=%s, typeLenBs=%d structname=%s, pointer=%d, arraySize=%d" %
              (self.name, self.type, self.typeLenBytes, self.structname, self.pointer, self.arraySize))

    def parseNode(self, ast_node):

        decl = ast_node
        typ = type(decl)

        #print("parseNode1: nodetype=%s" % typ)
        # print ("parseNode2: %s " % (c_ast.Typedef))
        try:
            if typ == c_ast.Typedef:
                try:
                    self.parseNode(decl.type)
                except:
                    print("parseNode failed!")
            elif typ == c_ast.TypeDecl or typ == c_ast.Typename or typ == c_ast.Decl:
                try:
                    self.parseNode(decl.type)
                except:
                    print("ParseNode case typ == c_ast.TypeDecl or typ == c_ast.Typename or typ == c_ast.Decl failed!")
            elif c_ast.PtrDecl == typ:
                try:
                    self.pointer += 1
                    self.parseNode(decl.type)
                except:
                    print("ParseNode:case PtrDecl exception!")
            elif typ == c_ast.IdentifierType:
                try:
                    '''
                        Key: char, short, int, long, unsigned char, unsigned short, unsigned int, unsigned long.
                    '''
                    declType = ' '.join(decl.names)

                    if declType not in typeList:
                        for node in self.c_ast.ext:
                            if type(node) == c_ast.Typedef and node.name == declType:
                                self.typedefName = declType
                                self.parseNode(node.type)
                    else:
                        self.type = ' '.join(decl.names)
                except:
                    print("ParseNode:case IdentifierType exception!")
            elif typ == c_ast.ArrayDecl:
                try:
                    self.arraySize = int(decl.dim.value)
                    self.parseNode(decl.type)
                except:
                    print("ParseNode:case ArrayDecl exception!")
            elif typ == c_ast.Struct:
                try:
                    self.type = "struct"
                    if type(decl.name) == type(None):
                        self.structname = " "
                    self.structname = decl.name
                except:
                    print("ParseNode:case struct exception!")
            else:
                print("No type matched for TYPE=%s!" % (typ))
        except:
            print("ParseNode failed! TYPE=%s!" % (typ))
            print(sys.exc_info()[0], sys.exc_info()[1])
        pass

    def parseNodeTypeLenBytes(self):
        self.typeLenBytes = 0
        try:
            self.typeLenBytes = self.IbcMsgLenParser.doParseTypeLenBytes(self.ast_node)
            if self.debug:
                print("self.typeLenBytes = %d" % self.typeLenBytes)
        except:
            print("Parse node type length failed!!! Nodename=%s" % self.name)
        return 0

    pass


class MsgH(c_ast.NodeVisitor):
    """
        OK
    """
    Dir = "./autoCoderTmp"
    MSG_H_PATH = Dir + "/msg.h"
    MSG_H_FD = None
    MSG_ID_CNT = 0xf000
    MSG_FUNC_ARGS_CNT = 0

    def __init__(self, FileAST, debug=False):
        self.c_ast = FileAST
        self.debug = debug
        try:
            self.MsgHInit()
            # self.visit(FileAST)
            self.iterASTGenerateMsgH()
            self.MsgHEnd()
        except Exception:
            print("StructVistor init failed!")

    def MsgHInit(self):
        if (not os.path.exists(self.Dir)):
            os.makedirs(self.Dir)
        try:
            self.MSG_H_FD = open(self.MSG_H_PATH, mode='w')
        except:
            return -1

        buf = self.MSG_H_FD
        buf.write("#ifndef __MSG_H\n")
        buf.write("#define __MSG_H\n\n")
        buf.write("#include <stdint.h>\n")
        buf.write("#include <sys/types.h>\n")
        buf.write("#define IN\n")
        buf.write("#define OUT\n")
        buf.write("#define INOUT\n")
        buf.write("#define ARRAY\n")

    def ParseStruct(self, struct_node, typedefName=""):
        """ Iter all struct in c_AST and parse it """
        global element
        if type(struct_node.decls) is type(None): return -1  # Avoid the AST reference struct node.

        if type(struct_node.name) != type(None) :
            node_name = struct_node.name
        else:
            node_name = " "

        buf = self.MSG_H_FD
        buf.write("\n#define MSG_ID_BASE_%s %#x\n" % (node_name if typedefName == "" else typedefName, self.MSG_ID_CNT))
        if typedefName == "":
            buf.write('struct %s {\n' % node_name)
        else:
            buf.write('typedef struct %s {\n' % node_name)

        for (child_name, child) in struct_node.children():
            try:
                #child.show()
                element = _explain_type(child, decl_name="\t" + child.name, instance=True)
                #print("Element type = %s (for %s)" % ( element, child.name))
            except:
                print("Get struct element %s type failed!" % child.name)
            buf.write("\t%s;\n" % element)

        if typedefName == "":
            buf.write('};\n')
        else:
            buf.write('} %s;\n' % typedefName)

        self.MSG_ID_CNT += 1
        return 0

    def ParseFuncDecl(self, func_node):
        """ Iter all struct in c_AST and parse it
                    (MSG'elements must be instance not reference)
                    #define MSG_ID_FUNC_set_if_speed_inmsg 				0x0
                    #define MSG_ID_FUNC_set_if_speed_outmsg 			0x1
                    struct set_if_speed_INMSG
                    {
                        int  port;
                        int  speed;
                        int  duplex;
                    };
                    struct set_if_speed_OUTMSG
                    {
                        int ___rv;
                    };
                """
        # FUNC_INMSG
        buf = self.MSG_H_FD
        node_name = func_node.type.declname
        buf.write("\n#define MSG_ID_FUNC_%s_inmsg %#x\n" % (node_name, self.MSG_FUNC_ARGS_CNT))
        buf.write('struct %s_INMSG {\n' % node_name)
        argsBuf = ""
        try:
            for param in func_node.args.params:
                if type(param.name) == type(None):
                    continue
                if param.name.find("OUT") == 0:
                    continue
                argsBuf += '\t'
                argsBuf += _explain_type(param, decl_name="\t" + param.name, instance=True)
                argsBuf += ';\n'
                # print argsBuf
        except:
            print("Iter func args failed!")
            return -1
        buf.write(argsBuf)
        buf.write('};\n')
        self.MSG_FUNC_ARGS_CNT += 1

        # FUNC_OUTMSG
        buf.write("\n#define MSG_ID_FUNC_%s_outmsg %#x\n" % (node_name, self.MSG_FUNC_ARGS_CNT))
        buf.write('struct %s_OUTMSG {\n' % node_name)
        argsBuf = ""
        for param in func_node.args.params:
            if type(param.name) == type(None):
                continue
            '''If arg name begin with IN, we thing it is not output args'''
            if param.name.find("IN") == 0 and param.name.find("INOUT") != 0:
                continue
            argsBuf += '\t'
            argsBuf += _explain_type(param, decl_name="\t" + param.name, instance=True)
            argsBuf += ';\n'
        argsBuf += '\tint ___rv;\n'
        buf.write(argsBuf)
        buf.write('};\n')

        self.MSG_FUNC_ARGS_CNT += 1
        return 0

    def ParseTypedefForBasetype(self, decl):
        typ = type(decl)
        if typ == c_ast.TypeDecl:
            try:
                if decl.declname:
                    newName = decl.declname
                else:
                    newName = ""
                return self.ParseTypedefForBasetype(decl.type) + newName
            except:
                print ("1")
        elif typ == c_ast.IdentifierType:
            quals = ""
            try:
                quals = ' '.join(decl.names) + ' ' if decl.names else ''
            except:
                print("2")
            return quals
        elif typ == c_ast.ArrayDecl:
            try:
                arrSize = int(decl.dim.value)
                buffer = self.ParseTypedefForBasetype(decl.type) + ("[%d]" % arrSize)
                #print (" typ == c_ast.ArrayDecl: %s" % buffer)
                return buffer
            except:
                print ("3")
        elif typ == c_ast.PtrDecl or typ == c_ast.Typename or typ == c_ast.Decl:
            return self.ParseTypedefForBasetype(decl.type)
        else:
            return ""
            pass
        pass

    def visit_Struct(self, struct_node):
        pass

    def visit_FuncDecl(self, func_node):
        pass

    def iterASTGenerateMsgH(self):
        for node in self.c_ast.ext:
            typ = type(node)
            if typ == c_ast.Typedef:
                typedefName = node.name
                typtyp = type(node.type.type)
                if typtyp == c_ast.Struct:
                    self.ParseStruct(node.type.type, typedefName=typedefName)
                else:
                    node.show(attrnames=True, nodenames=True, showcoord=False)
                    buffer = self.ParseTypedefForBasetype(node.type)
                    if buffer != "":
                        self.MSG_H_FD.write("typedef %s;\n" % buffer)
                    pass
            elif typ == c_ast.Decl:
                typtyp = type(node.type)
                if typtyp == c_ast.Struct:
                    self.ParseStruct(node.type)
                elif typtyp == c_ast.FuncDecl:
                    self.ParseFuncDecl(node.type)
            pass

    def MsgHEnd(self):
        if (self.MSG_H_FD is not None):
            return self.MSG_H_FD.write("#endif")

    pass


class MsgC(c_ast.NodeVisitor):
    """
        Not OK
    """
    Dir = "./autoCoderTmp"
    MSG_H_PATH = Dir + "/msg.c"
    MSG_H_FD = None

    def __init__(self, FileAST, debug=False):
        self.c_ast = FileAST
        self.debug = debug
        try:
            self.MsgCInit()
            # self.visit(FileAST)
            self.iterASTGenerateMsgC()
            self.MsgCEnd()
        except Exception:
            print("MsgC init failed!")

    def MsgCInit(self):
        if (not os.path.exists(self.Dir)):
            os.makedirs(self.Dir)
        try:
            self.MSG_H_FD = open(self.MSG_H_PATH, mode='w')
        except:
            return -1

        buf = self.MSG_H_FD
        buf.write("#include <stdio.h>\n")
        buf.write("#include <string.h>\n")
        buf.write("#include <stdlib.h>\n")
        buf.write("#include \"msg.h\"\n")
        buf.write("#include \"tlv_encode.h\"\n")
        buf.write("#include \"msg_func.h\"\n")
        buf.write("#include \"descripte.h\"\n")

    def MsgCEnd(self):
        pass

    def writeStructBASE_MSG_encode(self,struct_name, structMemList, typdefName=""):
        """ Iter all struct in c_AST and parse it
            int interface_info_t_BASE_MSG_encode(void * buf , int buf_len , void *obj)
            {
                struct interface_info_t* msg = (struct interface_info_t *)obj;
                void * buf_end = buf + buf_len;
                void * cur_buf= buf;
                int enc_len;
                enc_len = tlv_encode(cur_buf , buf_end - cur_buf , &msg->insert, make_simple_type(0 , TLV_TYPE_L32, TLV_TYPE_UNSIGNED));
                ck_len_and_move_buf_ptr(enc_len , cur_buf);
                enc_len = tlv_encode(cur_buf , buf_end - cur_buf , &msg->type, make_simple_type(1 , TLV_TYPE_L32, TLV_TYPE_UNSIGNED));
                ck_len_and_move_buf_ptr(enc_len , cur_buf);
                enc_len = tlv_encode_array(cur_buf , buf_end - cur_buf , &msg->interface_desc, make_simple_array_type(2 ,TLV_TYPE_L8,TLV_TYPE_UNSIGNED),256);
                ck_len_and_move_buf_ptr(enc_len , cur_buf);
                return cur_buf - buf;
            }
        """
        buf = self.MSG_H_FD

        if typdefName == "":
            buf.write("int %s_BASE_MSG_encode(void * buf , int buf_len , void *obj)\n{\n" % (struct_name))
            buf.write('\tstruct %s * msg = (struct %s *) obj;\n' % (struct_name, struct_name))
        else:
            buf.write("int %s_BASE_MSG_encode(void * buf , int buf_len , void *obj)\n{\n" % (typdefName))
            buf.write('\t%s * msg = (%s *) obj;\n' % (typdefName, typdefName))

        buf.write("\tvoid * buf_end = buf + buf_len;\n")
        buf.write("\tvoid * cur_buf = buf;\n")
        buf.write("\tint enc_len;\n\n")
        Elem_i = 0

        for strMemi in structMemList:
            tlv_encode_func = "tlv_encode"
            data_ptr = "&msg->insert"
            make_type = "make_simple_type(0 , TLV_TYPE_L32, TLV_TYPE_UNSIGNED)"
            tail = ");"

            memberName = strMemi.name

            if strMemi.arraySize > 0:
                try:
                    tlv_encode_func = "tlv_encode_array"
                    tail = (", %s);\n" % strMemi.arraySize)
                    data_ptr = "msg->%s" % memberName
                    tlv_type = "TLV_TYPE_L32"
                    if tlvDataTypeDict.has_key(strMemi.type):
                        tlv_type = tlvDataTypeDict[strMemi.type]
                    make_type = "make_simple_array_type(%d, %s, TLV_TYPE_UNSIGNED)" % (Elem_i, tlv_type)
                    pass
                except:
                    print("Array case failed!")
            else:
                try:
                    tlv_encode_func = "tlv_encode"
                    tail = ");\n"
                    data_ptr = "&msg->%s" % memberName
                    tlv_type = "TLV_TYPE_L32"
                    if tlvDataTypeDict.has_key(strMemi.type):
                        tlv_type = tlvDataTypeDict[strMemi.type]
                    else:
                        continue

                    if strMemi.type.find("struct") >= 0:
                        memberStructName = strMemi.structname if strMemi.typedefName == "" else strMemi.typedefName
                        make_type = "make_type(%d, MSG_ID_BASE_%s, TLV_TYPE_STRUCT, TLV_TYPE_UNSIGNED , 0)" % (
                            Elem_i, memberStructName)
                    else:
                        make_type = "make_simple_type(%d, %s, TLV_TYPE_UNSIGNED)" % (Elem_i, tlv_type)
                except:
                    print("Non-Array case failed!")

            buf.write("\tenc_len = " + tlv_encode_func + "(cur_buf, buf_end - cur_buf, " + data_ptr + ", " +
                      make_type + tail)
            buf.write("\tck_len_and_move_buf_ptr(enc_len, cur_buf);\n")
            Elem_i += 1

        buf.write("\n\treturn cur_buf - buf;\n}\n\n")
        pass

    def writeStructBASE_MSG_decode(self, struct_name,structMemList, typdefName=""):
        """
            int interface_info_t_BASE_MSG_decode(void * buf , int buf_len , void *obj , int obj_size)
            {
                struct interface_info_t* msg = (struct interface_info_t *)obj;
                int ___rv;
                if(obj_size < sizeof(struct interface_info_t))
                    return TLV_ERR_OOM;
                ___rv = tlv_decode(&msg->insert, sizeof(msg->insert), make_simple_type(0 , TLV_TYPE_L32 , TLV_TYPE_UNSIGNED),buf , buf_len);
                ck_decode_res(___rv);
                ___rv = tlv_decode(&msg->type, sizeof(msg->type), make_simple_type(1 , TLV_TYPE_L32 , TLV_TYPE_UNSIGNED),buf , buf_len);
                ck_decode_res(___rv);
                ___rv = tlv_decode(&msg->interface_desc, sizeof(msg->interface_desc), make_simple_array_type(2 , TLV_TYPE_L8, TLV_TYPE_UNSIGNED),buf , buf_len);
                ck_decode_res(___rv);
                return TLV_DECODE_SUCC;
            }
        """
        #print("writeStructBASE_MSG_decode 1")
        buf = self.MSG_H_FD

        if typdefName == "":
            buf.write(
                "int %s_BASE_MSG_decode(void * buf , int buf_len , void *obj, int obj_size)\n{\n" % (struct_name))
            buf.write('\tstruct %s * msg = (struct %s *) obj;\n\tint ___rv;\n' % (struct_name, struct_name))
            buf.write("\tif(obj_size < sizeof(struct %s))\n\t\treturn TLV_ERR_OOM;\n\n" % (struct_name))
        else:
            buf.write(
                "int %s_BASE_MSG_decode(void * buf , int buf_len , void *obj, int obj_size)\n{\n" % (typdefName))
            buf.write('\t%s * msg = (%s *) obj;\n\tint ___rv;\n' % (typdefName, typdefName))
            buf.write("\tif(obj_size < sizeof(%s))\n\t\treturn TLV_ERR_OOM;\n\n" % (typdefName))

        Elem_i = 0
        for strMemi in structMemList:
            data_ptr = "&msg->insert, sizeof(msg->insert)"
            make_type = "make_simple_type(0 , TLV_TYPE_L32, TLV_TYPE_UNSIGNED)"

            memberName = strMemi.name

            if strMemi.arraySize > 0:  # array
                data_ptr = "msg->%s, sizeof(msg->%s)" % (memberName, memberName)

                if tlvDataTypeDict.has_key(strMemi.type):
                    tlv_type = tlvDataTypeDict[strMemi.type]
                else:
                    print("MsgC-> member type parsed faield! %s" % strMemi.type)
                    return -1

                make_type = "make_simple_array_type(%s, %s, TLV_TYPE_UNSIGNED)" % (Elem_i, tlv_type)
                pass
            else:
                try:
                    data_ptr = "&msg->%s, sizeof(msg->%s)" % (memberName, memberName)
                    tlv_type = "TLV_TYPE_L32"

                    if strMemi.type == "struct" :
                        make_type = "make_type(%d, MSG_ID_BASE_%s, TLV_TYPE_STRUCT, TLV_TYPE_UNSIGNED , 0)" % (
                            Elem_i, strMemi.structname if strMemi.typedefName == "" else strMemi.typedefName)
                    else:
                        if tlvDataTypeDict.has_key(strMemi.type):
                            tlv_type = tlvDataTypeDict[strMemi.type]
                        else:
                            print("MsgC-> member type parsed faield!")
                            return -1
                        make_type = "make_simple_type(%s, %s, TLV_TYPE_UNSIGNED)" % (Elem_i, tlv_type)
                except:
                    pass
                pass

            buf.write("\t___rv = tlv_decode(" + data_ptr + ", " + make_type + ", buf, buf_len);\n")
            buf.write("\tck_decode_res(___rv);\n")
            Elem_i += 1

        buf.write("\n\treturn TLV_DECODE_SUCC;\n}\n")
        #print("writeStructBASE_MSG_decode 2 elem_i=%d" % Elem_i)
        pass

    def writeFuncINMSG_encode(self, func_name, args_list):
        """
            int tap_port_link_status_get_FUNC_INMSG_encode(void * buf , int buf_len , void *obj)
            {
                struct tap_port_link_status_get_INMSG * msg = (struct tap_port_link_status_get_INMSG *)obj;
                void * buf_end = buf + buf_len;
                void * cur_buf= buf;
                int enc_len;
                enc_len = tlv_encode(cur_buf , buf_end - cur_buf , &msg->slot_port_idx, make_simple_type(0 , TLV_TYPE_L32, TLV_TYPE_SIGNED));
                ck_len_and_move_buf_ptr(enc_len , cur_buf);
                enc_len = tlv_encode(cur_buf , buf_end - cur_buf , &msg->linkup, make_simple_type(1 , TLV_TYPE_L32, TLV_TYPE_SIGNED));
                ck_len_and_move_buf_ptr(enc_len , cur_buf);

                return cur_buf - buf;
            }
        """

        buf = self.MSG_H_FD
        buf.write("int %s_FUNC_INMSG_encode(void * buf , int buf_len , void *obj)\n{\n" % (func_name))
        buf.write('\tstruct %s_INMSG * msg = (struct %s_INMSG *) obj;\n' % (func_name, func_name))
        buf.write("\tvoid * buf_end = buf + buf_len;\n")
        buf.write("\tvoid * cur_buf = buf;\n")
        buf.write("\tint enc_len;\n\n")
        Elem_i = 0

        for param in args_list:

            memberName = param.name

            if param.type.find("void") == 0:
                continue

            '''Ignore output arguments'''
            if memberName.find("OUT") == 0:
                continue

            tlv_encode_func = "tlv_encode"
            data_ptr = "&msg->insert"
            make_type = "make_simple_type(0 , TLV_TYPE_L32, TLV_TYPE_UNSIGNED)"
            tail = ");"

            if param.arraySize > 0:  # array
                try:
                    tlv_encode_func = "tlv_encode_array"
                    tail = (", %s);\n" % param.arraySize)
                    data_ptr = "msg->%s" % memberName

                    if tlvDataTypeDict.has_key(param.type):
                        tlv_type = tlvDataTypeDict[param.type]
                    else:
                        print("func type invalid!")
                        return -1
                    make_type = "make_simple_array_type(%d, %s, TLV_TYPE_UNSIGNED)" % (Elem_i, tlv_type)
                    pass
                except:
                    print("Array case failed!")
            else:
                try:
                    tlv_encode_func = "tlv_encode"
                    tail = ");\n"
                    data_ptr = "&msg->%s" % memberName

                    if tlvDataTypeDict.has_key(param.type):
                        tlv_type = tlvDataTypeDict[param.type]
                    else:
                        print("func type invalid!")
                        return -1

                    try:
                        if param.type.find("struct") >= 0 :
                            memberStructName = param.structname if param.typedefName == "" else param.typedefName
                            make_type = "make_type(%d, MSG_ID_BASE_%s, TLV_TYPE_STRUCT, TLV_TYPE_UNSIGNED , 0)" % (
                                Elem_i, memberStructName)
                        else:
                            make_type = "make_simple_type(%d, %s, TLV_TYPE_UNSIGNED)" % (Elem_i, tlv_type)
                    except:
                        print("Bug 2Funcname=%s" % func_name)
                except:
                    print("Func encode Non-Array case failed! Funcname=%s" % func_name)

            buf.write("\tenc_len = " + tlv_encode_func + "(cur_buf, buf_end - cur_buf, " + data_ptr + ", " +
                      make_type + tail)
            buf.write("\tck_len_and_move_buf_ptr(enc_len, cur_buf);\n")
            Elem_i += 1
            pass

        buf.write("\n\treturn cur_buf - buf;\n}\n\n")
        pass

    def writeFuncINMSG_decode(self, func_name, args_list):
        buf = self.MSG_H_FD
        buf.write(
            "int %s_FUNC_INMSG_decode(void * buf , int buf_len , void *obj, int obj_size)\n{\n" % (func_name))
        buf.write('\tstruct %s_INMSG * msg = (struct %s_INMSG *) obj;\n\tint ___rv;\n' % (func_name, func_name))
        buf.write("\tif(obj_size < sizeof(struct %s_INMSG))\n\t\treturn TLV_ERR_OOM;\n\n" % (func_name))
        Elem_i = 0

        for param in args_list:

            memberName = param.name
            if param.type.find("void") == 0:
                continue
            '''Ignore output arguments'''
            if memberName.find("OUT") == 0:
                continue

            data_ptr = "&msg->insert, sizeof(msg->insert)"
            make_type = "make_simple_type(0 , TLV_TYPE_L32, TLV_TYPE_UNSIGNED)"

            if param.arraySize > 0:  # array
                try:
                    data_ptr = "msg->%s, sizeof(msg->%s)" % (memberName, memberName)
                    if tlvDataTypeDict.has_key(param.type):
                        tlv_type = tlvDataTypeDict[param.type]
                    else:
                        return -1
                    make_type = "make_simple_array_type(%s, %s, TLV_TYPE_UNSIGNED)" % (Elem_i, tlv_type)
                    pass
                except:
                    print("Array case failed!")
            else:
                try:
                    data_ptr = "&msg->%s, sizeof(msg->%s)" % (memberName, memberName)
                    tlv_type = "TLV_TYPE_L32"
                    if tlvDataTypeDict.has_key(param.type):
                        tlv_type = tlvDataTypeDict[param.type]

                    if param.type.find("struct") >= 0:
                        memberStructName = param.structname if param.typedefName == "" else param.typedefName
                        make_type = "make_type(%d, MSG_ID_BASE_%s, TLV_TYPE_STRUCT, TLV_TYPE_UNSIGNED , 0)" % (
                            Elem_i, memberStructName)
                    else:
                        make_type = "make_simple_type(%s, %s, TLV_TYPE_UNSIGNED)" % (Elem_i, tlv_type)
                except:
                    print("Non-Array case failed!")

            buf.write("\t___rv = tlv_decode(" + data_ptr + ", " + make_type + ", buf, buf_len);\n")
            buf.write("\tck_decode_res(___rv);\n")
            Elem_i += 1

        buf.write("\n\treturn TLV_DECODE_SUCC;\n}\n")
        pass

    def writeFuncOUTMSG_encode(self, func_name, args_list):
        """
            int tap_port_link_status_get_FUNC_INMSG_encode(void * buf , int buf_len , void *obj)
            {
                struct tap_port_link_status_get_INMSG * msg = (struct tap_port_link_status_get_INMSG *)obj;
                void * buf_end = buf + buf_len;
                void * cur_buf= buf;
                int enc_len;
                enc_len = tlv_encode(cur_buf , buf_end - cur_buf , &msg->slot_port_idx, make_simple_type(0 , TLV_TYPE_L32, TLV_TYPE_SIGNED));
                ck_len_and_move_buf_ptr(enc_len , cur_buf);
                enc_len = tlv_encode(cur_buf , buf_end - cur_buf , &msg->linkup, make_simple_type(1 , TLV_TYPE_L32, TLV_TYPE_SIGNED));
                ck_len_and_move_buf_ptr(enc_len , cur_buf);

                return cur_buf - buf;
            }
        """

        buf = self.MSG_H_FD
        buf.write("int %s_FUNC_OUTMSG_encode(void * buf , int buf_len , void *obj)\n{\n" % (func_name))
        buf.write('\tstruct %s_OUTMSG * msg = (struct %s_OUTMSG *) obj;\n' % (func_name, func_name))
        buf.write("\tvoid * buf_end = buf + buf_len;\n")
        buf.write("\tvoid * cur_buf = buf;\n")
        buf.write("\tint enc_len;\n\n")

        Elem_i = 0
        buf.write("\tenc_len = tlv_encode(cur_buf, buf_len, &msg->___rv, " +
                  "make_simple_type(0, TLV_TYPE_L32, TLV_TYPE_UNSIGNED));\n" +
                  "\tck_len_and_move_buf_ptr(enc_len, cur_buf);\n")
        Elem_i += 1
        for param in args_list:
            memberName = param.name
            if param.type.find("void") == 0:
                continue

            '''Ignore input arguments'''
            if memberName.find("IN") == 0 and memberName.find("INOUT") != 0:
                continue

            tlv_encode_func = "tlv_encode"
            data_ptr = "&msg->insert"
            make_type = "make_simple_type(0 , TLV_TYPE_L32, TLV_TYPE_UNSIGNED)"
            tail = ");"

            if param.arraySize > 0:  # array
                try:
                    tlv_encode_func = "tlv_encode_array"
                    tail = (", %s);\n" % param.arraySize)
                    data_ptr = "msg->%s" % memberName
                    if tlvDataTypeDict.has_key(param.type):
                        tlv_type = tlvDataTypeDict[param.type]
                    else:
                        return -1
                    make_type = "make_simple_array_type(%d, %s, TLV_TYPE_UNSIGNED)" % (Elem_i, tlv_type)
                    pass
                except:
                    print("Array case failed!")
            else:
                try:
                    tlv_encode_func = "tlv_encode"
                    tail = ");\n"
                    data_ptr = "&msg->%s" % memberName
                    if tlvDataTypeDict.has_key(param.type):
                        tlv_type = tlvDataTypeDict[param.type]
                    else:
                        return -1

                    try:
                        if param.type.find("struct") >= 0:
                            memberStructName = param.structname if param.typedefName == "" else param.typedefName
                            make_type = "make_type(%d, MSG_ID_BASE_%s, TLV_TYPE_STRUCT, TLV_TYPE_UNSIGNED , 0)" % (
                                Elem_i, memberStructName)
                        else:
                            make_type = "make_simple_type(%d, %s, TLV_TYPE_UNSIGNED)" % (Elem_i, tlv_type)
                    except:
                        print("Bug 2Funcname=%s" % func_name)
                except:
                    print("Func encode Non-Array case failed! Funcname=%s" % func_name)

            buf.write("\tenc_len = " + tlv_encode_func + "(cur_buf, buf_end - cur_buf, " + data_ptr + ", " +
                      make_type + tail)
            buf.write("\tck_len_and_move_buf_ptr(enc_len, cur_buf);\n")
            Elem_i += 1
            pass

        buf.write("\n\treturn cur_buf - buf;\n}\n\n")
        pass

    def writeFuncOUTMSG_decode(self, func_name, args_list):

        buf = self.MSG_H_FD
        buf.write(
            "int %s_FUNC_OUTMSG_decode(void * buf , int buf_len , void *obj, int obj_size)\n{\n" % (func_name))
        buf.write('\tstruct %s_OUTMSG * msg = (struct %s_OUTMSG *) obj;\n\tint ___rv;\n' % (func_name, func_name))
        buf.write("\tif(obj_size < sizeof(struct %s_OUTMSG))\n\t\treturn TLV_ERR_OOM;\n\n" % (func_name))

        buf.write("\t___rv = tlv_decode(&msg->___rv, sizeof(msg->___rv), make_simple_type(0, TLV_TYPE_L32, TLV_TYPE_UNSIGNED)," +
                  "buf, buf_len);\n\tck_decode_res(___rv);\n")
        Elem_i = 1
        for param in args_list:
            if param.type.find("void") == 0:
                continue
            memberName = param.name

            '''Ignore input arguments'''
            if memberName.find("IN") == 0 and memberName.find("INOUT") != 0:
                continue

            data_ptr = "&msg->insert, sizeof(msg->insert)"
            make_type = "make_simple_type(0 , TLV_TYPE_L32, TLV_TYPE_UNSIGNED)"

            if param.arraySize > 0:  # array
                try:
                    data_ptr = "msg->%s, sizeof(msg->%s)" % (memberName, memberName)
                    if tlvDataTypeDict.has_key(param.type):
                        tlv_type = tlvDataTypeDict[param.type]
                    else:
                        return -1
                    make_type = "make_simple_array_type(%s, %s, TLV_TYPE_UNSIGNED)" % (Elem_i, tlv_type)
                    pass
                except:
                    print("Array case failed!")
            else:
                try:
                    data_ptr = "&msg->%s, sizeof(msg->%s)" % (memberName, memberName)
                    tlv_type = "TLV_TYPE_L32"
                    if tlvDataTypeDict.has_key(param.type):
                        tlv_type = tlvDataTypeDict[param.type]
                    else:
                        return -1
                    if param.type.find("struct") >= 0:
                        memberStructName = param.structname if param.typedefName == "" else param.typedefName
                        make_type = "make_type(%d, MSG_ID_BASE_%s, TLV_TYPE_STRUCT, TLV_TYPE_UNSIGNED , 0)" % (
                            Elem_i, memberStructName)
                    else:
                        make_type = "make_simple_type(%s, %s, TLV_TYPE_UNSIGNED)" % (Elem_i, tlv_type)
                except:
                    print("Non-Array case failed!")

            buf.write("\t___rv = tlv_decode(" + data_ptr + ", " + make_type + ", buf, buf_len);\n")
            buf.write("\tck_decode_res(___rv);\n")
            Elem_i += 1

        buf.write("\n\treturn TLV_DECODE_SUCC;\n}\n")
        pass

    def ParseStruct(self, struct_node, typdefName=""):
        structMemList = []
        for (child_name, child) in struct_node.children():
            try:
                structMemi = memberDecl(child, child.name, self.c_ast)
            except:
                print("Parse struct chiled failed! childname=%s" % child.name)

            structMemList.append(structMemi)

        self.writeStructBASE_MSG_encode(struct_node.name, structMemList, typdefName=typdefName)
        self.writeStructBASE_MSG_decode(struct_node.name, structMemList, typdefName=typdefName)
        pass

    def ParseFuncDecl(self, func_node):
        args_list = []
        func_name = func_node.type.declname

        for param in func_node.args.params:
            if type(param.name) == type(None):
                continue
            try:
                arg_i = memberDecl(param, param.name, self.c_ast)
                args_list.append(arg_i)
            except:
                print("Parse func params failed!")

        self.writeFuncINMSG_encode(func_name, args_list)
        self.writeFuncINMSG_decode(func_name, args_list)
        self.writeFuncOUTMSG_encode(func_name, args_list)
        self.writeFuncOUTMSG_decode(func_name, args_list)
        pass

    def visit_Struct(self, struct_node):
        return 0

    def visit_FuncDecl(self, func_node):
        return 0

    def iterASTGenerateMsgC(self):
        for node in self.c_ast.ext:
            typ = type(node)

            if typ == c_ast.Typedef:
                #node.show()
                typedefName = node.name
                typtyp = type(node.type.type)
                if typtyp == c_ast.Struct:
                    self.ParseStruct(node.type.type, typdefName=typedefName)
                else:
                    continue

            elif typ == c_ast.Decl:
                typtyp = type(node.type)
                if typtyp == c_ast.Struct:
                    self.ParseStruct(node.type)
                elif typtyp == c_ast.FuncDecl:
                    self.ParseFuncDecl(node.type)
                    pass
                else:
                    continue
            else:
                continue
        pass

    pass


class MsgFuncH(c_ast.NodeVisitor):
    """
        OK
    """
    Dir = "./autoCoderTmp"
    PATH = Dir + "/msg_func.h"
    FD = None

    def __init__(self, FileAST, debug=False):
        self.c_ast = FileAST
        self.debug = debug
        try:
            self.MsgFuncHInit()
            # self.visit(FileAST)
            self.iterASTGenerateMsgfuncH()
            self.MsgFuncHEnd()
        except Exception:
            print("Create msg_func.h failed!")
        pass

    def MsgFuncHInit(self):
        if (not os.path.exists(self.Dir)):
            os.makedirs(self.Dir)

        try:
            self.FD = open(self.PATH, mode='w')
        except:
            return -1

        buf = self.FD
        buf.write("#ifndef __MSG_FUNC_H\n")
        buf.write("#define __MSG_FUNC_H\n")
        pass

    def MsgFuncHEnd(self):
        if (self.FD is not None):
            buf = self.FD
            buf.write("#endif\n")
        pass

    def ParseStruct(self, struct_node, typdefName=""):
        if type(struct_node.decls) is type(None): return -1

        try:
            buf = self.FD
            struct_name = struct_node.name if typdefName == "" else typdefName

            buf.write("static int %s_BASE_MSG_encode(void * buf , int buf_len , void *obj);\n" % (struct_name))
            buf.write(
                "static int %s_BASE_MSG_decode(void * buf , int buf_len , void *obj , int obj_size);\n" % (struct_name))
        except:
            print("visit struct failed!")

    def ParseFuncDecl(self, func_node):
        try:
            buf = self.FD
            node_name = func_node.type.declname
            buf.write("static int %s_FUNC_INMSG_encode(void * buf , int buf_len , void *obj);\n" % (node_name))
            buf.write(
                "static int %s_FUNC_INMSG_decode(void * buf , int buf_len , void *obj , int obj_size);\n" % (node_name))
            buf.write("static int %s_FUNC_OUTMSG_encode(void * buf , int buf_len , void *obj);\n" % (node_name))
            buf.write("static int %s_FUNC_OUTMSG_decode(void * buf , int buf_len , void *obj ,int obj_size);\n" %
                      (node_name))
        except:
            print("visit funcDecl failed!")

    def visit_Struct(self, struct_node):
        pass

    def visit_FuncDecl(self, func_node):
        pass

    def iterASTGenerateMsgfuncH(self):
        for node in self.c_ast.ext:
            typ = type(node)

            if typ == c_ast.Typedef:
                #node.show()
                typedefName = node.name
                typtyp = type(node.type.type)
                if typtyp == c_ast.Struct:
                    self.ParseStruct(node.type.type, typdefName=typedefName)
                else:
                    continue

            elif typ == c_ast.Decl:
                typtyp = type(node.type)
                if typtyp == c_ast.Struct:
                    self.ParseStruct(node.type)
                elif typtyp == c_ast.FuncDecl:
                    self.ParseFuncDecl(node.type)
                    pass
                else:
                    continue
            else:
                continue
        pass

    pass


class ExampleH(c_ast.NodeVisitor):
    """
        OK
    """
    Dir = "./autoCoderTmp"
    PATH = Dir + "/example.h"
    FD = None

    def __init__(self, FileAST, debug=False):
        self.c_ast = FileAST
        self.debug = debug
        try:
            self.ExampleHInit()
            # self.visit(FileAST)
            self.iterASTGenerateExampleH()
            self.ExampleHEnd()
        except:
            print("Create example.h failed!")
        pass

    def ExampleHInit(self):
        if (not os.path.exists(self.Dir)):
            os.makedirs(self.Dir)

        try:
            self.FD = open(self.PATH, mode='w')
        except:
            return -1

        buf = self.FD
        buf.write("#ifndef __EXAMPLE_H\n")
        buf.write("#define __EXAMPLE_H\n")
        pass

    def ExampleHEnd(self):
        if (self.FD is not None):
            buf = self.FD
            buf.write("#endif\n")
        pass

    def visit_FuncDecl(self, func_node):
        pass

    def ParseFuncDecl(self, func_node):

        """
            int set_if_speed_called(ibc_trans_handle_t trans);
            int set_if_speed_call(ibc_addr_t addr,IN int port, IN int speed, IN int duplex);
        """
        if (self.FD is None):
            print("FD not init!")
            return -1
        #func_node.show(attrnames=True, nodenames=True, showcoord=False)

        buf = self.FD
        buf.write("int %s_called(ibc_trans_handle_t trans);\n" % func_node.type.declname)
        buf.write("int %s_call(ibc_addr_t addr" % func_node.type.declname)

        argsBuf = ""
        try:
            for param in func_node.args.params:
                if type(param.name) == type(None):
                    continue
                argsBuf += ', '
                argsBuf += _explain_type(param, decl_name="\t" + param.name)
            argsBuf += ');\n'
            buf.write(argsBuf)
        except:
            print("Parse func failed for example.h!")
        pass

    def iterASTGenerateExampleH(self):
        for node in self.c_ast.ext:
            if type(node) == c_ast.Decl and type(node.type) == c_ast.FuncDecl:
                self.ParseFuncDecl(node.type)

    pass


class ExampleC(c_ast.NodeVisitor):
    """
        OK
    """
    Dir = "./autoCoderTmp"
    PATH = Dir + "/example.c"
    FD = None

    def __init__(self, FileAST, debug=False):
        self.debug = debug
        self.c_ast = FileAST
        try:
            self.ExampleCInit()
            # self.visit(FileAST)
            self.iterASTGenerateExampleC()
            self.ExampleCEnd()
        except:
            print("Create example.c failed!")
        pass

    def ExampleCInit(self):
        if (not os.path.exists(self.Dir)):
            os.makedirs(self.Dir)
        try:
            self.FD = open(self.PATH, mode='w')
        except:
            return -1

        buf = self.FD
        buf.write("#include<stdio.h>\n")
        buf.write("#include<stdlib.h>\n")
        buf.write("#include<string.h>\n")
        buf.write("#include \"msg.h\"\n")
        buf.write("#include \"ibc_addr.h\"\n")
        buf.write("#include \"tlv_encode.h\"\n")
        buf.write("#include \"ibc.h\"\n")
        buf.write("#include \"ibc_error.h\"\n")
        buf.write("#include \"func_name.h\"\n")
        pass

    def ExampleCEnd(self):
        pass

    def writeFuncIbcCall(self, func_name, args_list):
        if self.FD is None:
            print("FD not init!")
            return -1

        buf = self.FD
        buf.write("int %s_call(ibc_addr_t addr" % func_name)

        ## Fisrt line: int set_if_speed_call(ibc_addr_t addr,int IN_port,int IN_speed,int IN_duplex)
        func_decl_argsBuf = ""
        for arg_i in args_list:
            func_decl_argsBuf += ", "

            # type
            typeStr = ""
            if arg_i.type == "struct":
                typeStr += "struct %s " % arg_i.structname
            else:
                typeStr += "%s " % arg_i.type
            func_decl_argsBuf += typeStr

            # pointer
            if arg_i.pointer > 0:
                for i in range(arg_i.pointer):
                    func_decl_argsBuf += "*"
            # name
            func_decl_argsBuf += "%s" % arg_i.name

            # array
            if arg_i.arraySize > 0:
                func_decl_argsBuf += "[%d]" % arg_i.arraySize
            pass
        func_decl_argsBuf += ")\n{\n"
        buf.write("%s" % func_decl_argsBuf)

        buf.write("\tibc_trans_handle_t trans;\n\tvoid * send_buf, *recv_buf;\n\tint buf_len;\n")
        buf.write("\tstruct %s_INMSG inmsg;\n" % func_name)

        # part 2: msg init
        msgInitBuf = "\t"
        for arg_i in args_list:
            if arg_i.name.find("OUT") == 0:
                continue

            if arg_i.arraySize > 0:  # array
                if self.debug:
                    print("arg_i.typeLenBytes = %d, ArraySize = %d" % (arg_i.typeLenBytes, arg_i.arraySize))
                msgInitBuf += "memcpy(inmsg.%s, %s, %d);\n\t" % (
                    arg_i.name, arg_i.name, arg_i.typeLenBytes * arg_i.arraySize)
            else:
                if arg_i.pointer > 0:
                    msgInitBuf += "inmsg.%s = *%s;\n\t" % (arg_i.name, arg_i.name)
                else:
                    msgInitBuf += "inmsg.%s = %s;\n\t" % (arg_i.name, arg_i.name)
            pass
        msgInitBuf += "struct %s_OUTMSG outmsg={0};" % func_name
        buf.write("%s\n" % msgInitBuf)

        # part 3: msg size
        msgSizeBuf = ""
        msgSizeBuf += "\n\tint e, array_i;"
        msgSizeBuf += "\n\ttrans = ibc_new_trans(IBC_TRANS_CMD);"
        msgSizeBuf += "\n\tif (trans == NULL)"
        msgSizeBuf += "\n\t{"
        msgSizeBuf += "\n\t\tibc_err_msg(\"@%s:Create ibc transacation failed\\n\", __func__);"
        msgSizeBuf += "\n\t\treturn IBC_E_OPEN;"
        msgSizeBuf += "\n\t}"
        msgSizeBuf += "\n\tbuf_len = tlv_encode_msg_size(MSG_ID_FUNC_%s_inmsg);" % func_name
        buf.write("%s\n" % msgSizeBuf)

        # part 4: tlv_encode
        msgSizeBuf = ""
        msgSizeBuf += "\n\tif (buf_len < 0)"
        msgSizeBuf += "\n\t{"
        msgSizeBuf += "\n\t\tibc_err_msg(\"@%s : Cannot ensure the required buffer size\\n\", __func__);"
        msgSizeBuf += "\n\t\treturn IBC_E_MEM;"
        msgSizeBuf += "\n\t}"
        msgSizeBuf += "\n\tsend_buf = ibc_trans_alloc_sendbuf(trans, buf_len);"
        msgSizeBuf += "\n\tif (send_buf == NULL)"
        msgSizeBuf += "\n\t{"
        msgSizeBuf += "\n\t\tibc_err_msg(\"@%s:Allocate send buffer failed\\n\", __func__);"
        msgSizeBuf += "\n\t\treturn IBC_E_MEM;"
        msgSizeBuf += "\n\t}"
        msgSizeBuf += "\n\tif (tlv_encode(send_buf, buf_len, & inmsg, " + \
                      "make_cmd_type(MSG_ID_FUNC_%s_inmsg)) < 0)" % func_name

        buf.write("%s\n" % msgSizeBuf)

        # part 5: tlv_decode
        msgSizeBuf = ""
        msgSizeBuf += "\n\t{"
        msgSizeBuf += "\n\t\tibc_err_msg(\"@%s:Encode inmsg failed\\n\",__func__);"
        msgSizeBuf += "\n\t\treturn IBC_E_ENCODE;"
        msgSizeBuf += "\n\t}"
        msgSizeBuf += "\n\tif((e = ibc_trans_request(trans, addr)) < 0)"
        msgSizeBuf += "\n\t{"
        msgSizeBuf += "\n\t    ibc_err_msg(\"@%s:ibc transacation request failed e=%d\\n\",__func__,e);"
        msgSizeBuf += "\n\t    ibc_free_trans(trans);"
        msgSizeBuf += "\n\t    return e;"
        msgSizeBuf += "\n\t}"
        msgSizeBuf += "\n\tif((e = ibc_trans_get_rcvbuf( trans, &recv_buf,  &buf_len) )< 0)"
        msgSizeBuf += "\n\t{"
        msgSizeBuf += "\n\t    ibc_err_msg(\"@%s:get recv buf failed\\n\",__func__);"
        msgSizeBuf += "\n\t    ibc_free_trans(trans);"
        msgSizeBuf += "\n\t    return e;"
        msgSizeBuf += "\n\t}"
        msgSizeBuf += "\n\tif((e = tlv_decode(&outmsg , sizeof(outmsg)," + \
                      "make_cmd_type(MSG_ID_FUNC_%s_outmsg )" % func_name + \
                      ", recv_buf , buf_len) )!= TLV_DECODE_SUCC)"
        buf.write("%s\n" % msgSizeBuf)
        # part 6: return
        msgSizeBuf = ""
        msgSizeBuf += "\n\t{"
        msgSizeBuf += "\n\t    ibc_err_msg(\"@%s:resolve ret buffer failed \\n\", __func__);"
        msgSizeBuf += "\n\t}"
        msgSizeBuf += "\n\tibc_free_trans(trans);"

        for arg_i in args_list:
            if arg_i.name.find("IN") == 0 and arg_i.name.find("INOUT") != 0:
                continue

            if arg_i.arraySize > 0:  # array
                msgSizeBuf += "\n\tmemcpy(%s, outmsg.%s, %d);" % (
                    arg_i.name, arg_i.name, arg_i.typeLenBytes * arg_i.arraySize)
            else:
                if arg_i.pointer > 0:
                    msgSizeBuf += "\n\t*%s = outmsg.%s;" % (arg_i.name, arg_i.name)
                else:
                    msgSizeBuf += "\n\t%s = outmsg.%s;" % (arg_i.name, arg_i.name)

        msgSizeBuf += "\n\treturn outmsg.___rv;"
        buf.write("%s\n" % msgSizeBuf)
        ## Last line
        buf.write("}\n")
        pass

    def writeFuncIbcCalled(self, func_name, args_list):
        if self.FD is None:
            print("FD not init!")
            return -1

        buf = self.FD
        buf.write("int %s_called(ibc_trans_handle_t trans)\n{" % func_name)

        ## Fisrt line: int set_if_speed_call(ibc_addr_t addr,int IN_port,int IN_speed,int IN_duplex)

        buf.write("\n\tvoid * send_buf ,*recv_buf;\n\tint buf_len, array_i;")
        buf.write("\n\tstruct %s_INMSG inmsg;" % func_name)
        buf.write("\n\tstruct %s_OUTMSG outmsg;" % func_name)

        # part 2: msg init
        buf.write("\n\tmemset( & inmsg, 0, sizeof(inmsg));")
        buf.write("\n\tmemset( & outmsg, 0, sizeof(outmsg));")

        # part 3: get recevice buffer
        buf.write("\n\tif (ibc_trans_get_rcvbuf(trans, & recv_buf, & buf_len) < 0)")
        buf.write("\n\t{")
        buf.write("\n\t    ibc_err_msg(\"@%s: Get recv buffer failed\\n\", __func__);")
        buf.write("\n\t    return IBC_E_MEM;")
        buf.write("\n\t}")

        # part 4: tlv_decode
        msgSizeBuf = ""
        msgSizeBuf += "\n\tif(tlv_decode(&inmsg, sizeof(inmsg)," \
                      " make_cmd_type(MSG_ID_FUNC_%s_inmsg), " \
                      "recv_buf , buf_len) < 0)" % func_name
        msgSizeBuf += "\n\t{"
        msgSizeBuf += "\n\t\tibc_err_msg(\"@%s: Decode inmsg failed\\n\",__func__);"
        msgSizeBuf += "\n\t\treturn IBC_E_FORMAT;"
        msgSizeBuf += "\n\t}"
        buf.write(msgSizeBuf)

        # part 5: call real function : outmsg.___rv = set_if_speed(inmsg.port,inmsg.speed,inmsg.duplex);
        msgSizeBuf = ""
        msgSizeBuf += "\n\toutmsg.___rv = %s(" % func_name

        try:
            for arg_i in args_list:
                index = args_list.index(arg_i)
                if index > 0:
                    msgSizeBuf += ", "

                if arg_i.pointer > 0 and arg_i.arraySize == 0:
                    if arg_i.name.find("OUT") == 0:
                        msgSizeBuf += "&outmsg.%s" % arg_i.name
                    else:
                        msgSizeBuf += "&inmsg.%s" % arg_i.name
                else:
                    msgSizeBuf += "inmsg.%s" % arg_i.name
            msgSizeBuf += ");"
            for arg_i in args_list:
                if arg_i.name.find("INOUT") == 0:
                    if arg_i.arraySize > 0:  # array
                        msgSizeBuf += "\n\tmemcpy(outmsg.%s, inmsg.%s, %d);" % (
                            arg_i.name, arg_i.name, arg_i.typeLenBytes * arg_i.arraySize)
                    else:
                        msgSizeBuf += "\n\toutmsg.%s = inmsg.%s;" % (arg_i.name, arg_i.name)
        except:
            print("parse real function args failed!")
        buf.write("%s\n" % msgSizeBuf)

        # part 6: msg size
        msgSizeBuf = ""
        msgSizeBuf += "\n\tbuf_len = tlv_encode_msg_size(MSG_ID_FUNC_%s_outmsg);" % func_name
        msgSizeBuf += "\n\tif (buf_len < 0)"
        msgSizeBuf += "\n\t{"
        msgSizeBuf += "\n\t\tibc_err_msg(\"@%s : Cannot ensure the required buffer size\\n\", __func__);"
        msgSizeBuf += "\n\t\treturn IBC_E_MEM;"
        msgSizeBuf += "\n\t}"
        msgSizeBuf += "\n\tsend_buf = ibc_trans_alloc_sendbuf(trans, buf_len);"
        msgSizeBuf += "\n\tif (send_buf == NULL)"
        msgSizeBuf += "\n\t{"
        msgSizeBuf += "\n\tibc_err_msg(\"@%s:Allocate sendbuffer failed\\n\", __func__);"
        msgSizeBuf += "\n\treturn IBC_E_MEM;"
        msgSizeBuf += "\n\t}"
        buf.write("%s\n" % msgSizeBuf)

        # part 7: encode
        msgSizeBuf = ""
        msgSizeBuf += "\n\tif (tlv_encode(send_buf, buf_len, &outmsg, make_cmd_type(MSG_ID_FUNC_%s_outmsg)) < 0)" % func_name
        msgSizeBuf += "\n\t{"
        msgSizeBuf += "\n\t\tibc_err_msg(\"@%s:Encode out msg faield\\n\", __func__);"
        msgSizeBuf += "\n\t\treturn IBC_E_ENCODE;"
        msgSizeBuf += "\n\t}"
        msgSizeBuf += "\n\tif (ibc_trans_reply(trans) < 0)"
        msgSizeBuf += "\n\t{"
        msgSizeBuf += "\n\t\tibc_err_msg(\"@%s:Send transacation reply failed\\n\", __func__);"
        msgSizeBuf += "\n\t\treturn IBC_E_SEND;"
        msgSizeBuf += "\n\t}"
        buf.write("%s\n" % msgSizeBuf)

        ## Last line
        buf.write("\n\treturn IBC_E_NONE;")
        buf.write("\n}\n")
        pass

    def iterASTGenerateExampleC(self):
        for node in self.c_ast.ext:
            if type(node) == c_ast.Decl and type(node.type) == c_ast.FuncDecl:
                self.ParseFuncDecl(node.type)
                pass
        pass

    def ParseFuncDecl(self, func_node):
        args_list = []
        func_name = func_node.type.declname

        for param in func_node.args.params:
            if type(param.name) == type(None):
                continue
            try:
                arg_i = memberDecl(param, param.name, self.c_ast)
                args_list.append(arg_i)
            except:
                print("Parse func params failed!")

        self.writeFuncIbcCall(func_name, args_list)
        self.writeFuncIbcCalled(func_name, args_list)
        pass

    def visit_FuncDecl(self, func_node):
        pass

    pass


class DescripteH(c_ast.NodeVisitor):
    Dir = "./autoCoderTmp"
    PATH = Dir + "/descripte.h"
    FD = None
    MSG_DESCRIPTOR_CNT = 1

    def __init__(self, FileAST, debug=False):
        self.c_ast = FileAST
        try:
            self.debug = debug
            self.IbcMsgParser = ElemIbcMsgLenParser(FileAST=self.c_ast)

            self.DescripteHInit()
            # self.visit(FileAST)
            self.iterASTGenerateDescripte()
            self.DescripteHEnd()

        except:
            print("*** Create descripte.h failed! ***")
        pass

    def DescripteHInit(self):

        if not os.path.exists(self.Dir):
            os.makedirs(self.Dir)

        try:
            self.FD = open(self.PATH, mode='w')
        except:
            return -1

        buf = self.FD
        buf.write("#ifndef __DESCRIPTE_H\n")
        buf.write("#define __DESCRIPTE_H\n")
        buf.write("#include \"msg.h\"\n")
        buf.write("#include \"tlv_encode.h\"\n")
        buf.write("#include \"msg_func.h\"\n")
        buf.write("static MSG_DESCRIPTOR global_msg_descriptors[] = \n{\n")
        pass

    def DescripteHEnd(self):
        if self.FD is not None:
            buf = self.FD
            buf.write("\t{\n\t\tNULL,\n\t\t0,\n\t\t0,\n\t\t0,\n\t\tNULL,\n\t\tNULL,\n\t\tNULL,\n\t}\n")
            buf.write("};\n")
            buf.write("MSG_DESCRIPTOR * global_msg_desc_ptr = &global_msg_descriptors[0];\n")
            buf.write("#endif")

    def StructIbcBufLengthCalculate(self, struct_node):
        length = 0
        try:
            length += self.IbcMsgParser.doParse(struct_node)
        except Exception:
            print("StructIbcBufLengthCalculate failed!****")

        if self.debug:
            print("Struct %s length=%d" % (struct_node.name, length))

        return length

    def FuncArgsIbcBufLengthCalculate(self, func_node, inmsg=False, outmsg=False):
        length = 16
        try:
            #func_node.show(attrnames=True, nodenames=True, showcoord=False)
            #print ("1 length = %d (%s)" % (length, func_node.type.declname))

            for param in func_node.args.params:
                if type(param.name) == type(None):
                    if outmsg :
                        length += 8
                    continue

                if inmsg:
                    if param.name.find("OUT") == 0:
                        continue

                if outmsg:
                    if param.name.find("IN") == 0 and \
                                    param.name.find("INOUT") != 0:
                        continue
                #print ("length = %d" % length)
                length += self.IbcMsgParser.doParse(param, onlyDataLen=False)
                #print ("Func %s : param %s, length=%d" %(func_node.type.declname,param.name,length))

        except Exception:
            print("FuncArgsIbcBufLengthCalculate failed!****")

        if self.debug:
            print("Func %s ibcmsgbuf length = %d" % (func_node.type.declname, length))
            pass

        return length

    def ParseStruct(self, struct_node, typdefName=""):
        """
                    {
                        "interface_info_t",
                        MSG_ID_BASE_interface_info_t,
                        sizeof(struct interface_info_t),
                        288,
                        interface_info_t_BASE_MSG_encode,
                        interface_info_t_BASE_MSG_decode,
                        &global_msg_descriptors[1],
                    },
                """
        if type(struct_node.decls) is type(None): return -1

        ibc_buf_len = self.StructIbcBufLengthCalculate(struct_node)

        try:
            # struct_node.show(attrnames=True, nodenames=True, showcoord=False)

            if typdefName != "":
                node_name = typdefName
            else:
                node_name = struct_node.name

            buf = self.FD
            buf.write("\t{\n")
            buf.write("\t\t\"%s\",\n" % node_name)
            buf.write("\t\tMSG_ID_BASE_%s,\n" % node_name)
            if typdefName == "":
                buf.write("\t\tsizeof(struct %s),\n" % node_name)
            else:
                buf.write("\t\tsizeof(%s),\n" % node_name)

            buf.write("\t\t%d,\n" % ibc_buf_len)
            buf.write("\t\t%s_BASE_MSG_encode,\n" % node_name)
            buf.write("\t\t%s_BASE_MSG_decode,\n" % node_name)
            buf.write("\t\t&global_msg_descriptors[%d],\n" % self.MSG_DESCRIPTOR_CNT)
            buf.write("\t},\n")
        except Exception:
            print("Descripte.visit_Struct failed!!!")
            return -1
        self.MSG_DESCRIPTOR_CNT += 1
        pass

    def visit_Struct(self, struct_node):
        pass

    def visit_FuncDecl(self, func_node):
        pass

    def ParseFuncDecl(self, func_node, typdefName=""):
        try:
            ibc_buf_len = self.FuncArgsIbcBufLengthCalculate(func_node, inmsg=True)
            buf = self.FD
            node_name = func_node.type.declname
            buf.write("\t{\n")
            buf.write("\t\t\"%s_INMSG\",\n" % node_name)
            buf.write("\t\tMSG_ID_FUNC_%s_inmsg,\n" % node_name)
            buf.write("\t\tsizeof(struct %s_INMSG),\n" % node_name)
            buf.write("\t\t%d,\n" % ibc_buf_len)
            buf.write("\t\t%s_FUNC_INMSG_encode,\n" % node_name)
            buf.write("\t\t%s_FUNC_INMSG_decode,\n" % node_name)
            buf.write("\t\t&global_msg_descriptors[%d],\n" % self.MSG_DESCRIPTOR_CNT)
            buf.write("\t},\n")
            self.MSG_DESCRIPTOR_CNT += 1

            ibc_buf_len = self.FuncArgsIbcBufLengthCalculate(func_node, outmsg=True)
            buf.write("\t{\n")
            buf.write("\t\t\"%s_OUTMSG\",\n" % node_name)
            buf.write("\t\tMSG_ID_FUNC_%s_outmsg,\n" % node_name)
            buf.write("\t\tsizeof(struct %s_OUTMSG),\n" % node_name)
            buf.write("\t\t%d,\n" % ibc_buf_len)
            buf.write("\t\t%s_FUNC_OUTMSG_encode,\n" % node_name)
            buf.write("\t\t%s_FUNC_OUTMSG_decode,\n" % node_name)
            buf.write("\t\t&global_msg_descriptors[%d],\n" % self.MSG_DESCRIPTOR_CNT)
            buf.write("\t},\n")
            self.MSG_DESCRIPTOR_CNT += 1
        except Exception:
            print("Descripte.visit_FuncDecl failed!!!")
            return -1

        pass

    def iterASTGenerateDescripte(self):
        for node in self.c_ast.ext:
            typ = type(node)

            if typ == c_ast.Typedef:
                #node.show()
                typedefName = node.name
                typtyp = type(node.type.type)
                if typtyp == c_ast.Struct:
                    self.ParseStruct(node.type.type, typdefName=typedefName)
                else:
                    continue

            elif typ == c_ast.Decl:
                typtyp = type(node.type)
                if typtyp == c_ast.Struct:
                    self.ParseStruct(node.type)
                elif typtyp == c_ast.FuncDecl:
                    self.ParseFuncDecl(node.type)
                    pass
                else:
                    continue
            else:
                continue

        pass

    pass


class IbcInitC(c_ast.NodeVisitor):
    Dir = "./autoCoderTmp"
    PATH = Dir + "/ibc_init.c"
    FD = None

    def __init__(self, FileAST, debug=False):
        self.c_ast = FileAST
        try:
            self.debug = debug
            #self.IbcMsgParser = ElemIbcMsgLenParser(FileAST=self.c_ast)

            self.IbcInitCInit()
            # self.visit(FileAST)
            self.iterASTGenerateIbcInitC()
            self.IbcInitCEnd()

        except:
            print("*** Create descripte.h failed! ***")
        pass

    def IbcInitCInit(self):

        if not os.path.exists(self.Dir):
            os.makedirs(self.Dir)

        try:
            self.FD = open(self.PATH, mode='w')
        except:
            return -1

        buf = self.FD
        buf.write("#include \"msg.h\"\n")
        buf.write("#include \"ibc_addr.h\"\n")
        buf.write("#include \"ibc.h\"\n")
        buf.write("#include \"msg.h\"\n")
        buf.write("#include \"example.h\"\n")

        buf.write("\nvoid ibc_callback_register(void)\n{")
        pass

    def IbcInitCEnd(self):
        if self.FD is not None:
            buf = self.FD
            buf.write("\n};\n")
    pass

    def ParseFuncDecl(self, func_node):
        try:
            node_name = func_node.type.declname
            self.FD.write("\n\tibc_register_trans_callback(MSG_ID_FUNC_%s_inmsg , %s_called);" % (node_name,node_name))
        except:
            print ("iterASTGenerateIbcInitC failed!")
        pass

    def iterASTGenerateIbcInitC(self):
        for node in self.c_ast.ext:
            typ = type(node)

            if typ == c_ast.Decl:
                typtyp = type(node.type)
                if typtyp == c_ast.FuncDecl:
                    self.ParseFuncDecl(node.type)
                    pass
                else:
                    continue
            else:
                continue

        pass



class CHeaderParser(object):
    def __init__(self, debug=False):
        self.debug = debug
        pass

    def ParseFile(self, CHeaderFile):
        a_s_t = None
        try:
            print("C header file is %s" % CHeaderFile)
            a_s_t = parse_file(CHeaderFile)
            if self.debug: a_s_t.show(attrnames=True, nodenames=True, showcoord=False)
        except Exception:
            print("parse_file %s failed!" % CHeaderFile)
            return None
        return a_s_t

    def ParseBuf(self, CHeaderBuf=None, CHeaderBufLen=None):
        pass

    pass


class TypedefVisitor(c_ast.NodeVisitor):
    def __init__(self):
        print("Define TypedefVisitor.")
        pass

    def visit_Typedef(self, node):
        print("Typedef node : %s" % node.name)

    pass


class AutoCoder(object):
    """
    Auto generate some C files according to FileAST.
    """

    def __init__(self, FileAST=None):
        self.msg_h = 0
        self.msg_func_h = 0
        self.msg_c = 0
        self.example_h = 0
        self.example_c = 0
        self.descripte_h = 0
        self.ibc_init_h = 0
        self.c_ast = FileAST

    '''Create msg.h OK '''

    def GenarateMsgH(self, debug=False):
        """
        Generate msg.h file.
            1. This file has defined the struct and it`s elements.
            2. the macro which is specific the msg id for struct define here.
        Returns
        -------
            self.msg_h
            1 : success - msg.h has been created.
            0 : failed  - msg.h create failed.
        """
        if self.c_ast is None:
            print("Not init this object with AST!")
            return 0

        try:
            MsgH(self.c_ast, debug=debug)
        except Exception:
            self.msg_h = 0
            return 0

        self.msg_h = 1
        return self.msg_h

    '''Create msg_func.h OK '''

    def GenerateMsgFuncH(self, debug=False):
        if self.c_ast is None:
            print("Not init this object with AST!")
            return 0
        try:
            MsgFuncH(self.c_ast, debug=debug)
        except:
            print("Create msg_func.h failed!")
            self.msg_func_h = 0
            return 0

        self.msg_func_h = 1
        return self.msg_func_h

    '''Create msg.c OK '''

    def GenerateMsgC(self, debug=False):
        if self.c_ast is None:
            print("Not init this object with AST!")
            return 0
        try:
            MsgC(self.c_ast, debug=debug)
            self.msg_c = 1
        except:
            print("Create msg.c failed!****")
        pass

    '''Create example.h OK '''

    def GenerateExampleH(self, debug=False):
        if self.c_ast is None:
            print("Not init this object with AST!")
            return 0

        try:
            ExampleH(self.c_ast, debug=debug)
        except:
            print("Create example.h failed!")
            self.example_h = 0
            return -1

        self.example_h = 1
        pass

    '''Create example.c OK '''

    def GenerateExampleC(self, debug=False):
        if self.c_ast is None:
            print("Not init this object with AST!")
            return 0
        try:
            if ExampleC(self.c_ast, debug=debug) < 0:
                return -1
            self.example_c = 1
        except:
            print("Create example.c failed!")
        pass

    '''Create descripte.h OK '''

    def GenerateDescripteH(self, debug=False):
        if self.c_ast is None:
            print("Not init this object with AST!")
            return 0

        try:
            if DescripteH(self.c_ast, debug=debug) < 0:
                return -1
        except:
            print("*** Create descripte.h failed! ***")
            self.descripte_h = 0
            return -1

        self.descripte_h = 1
        pass

    '''Create ibc_init.c OK '''

    def GenerateIbcInitC(self, debug=False):
        if self.c_ast is None:
            print("Not init this object with AST!")
            return 0

        try:
            if IbcInitC(self.c_ast, debug=debug) < 0:
                return -1
        except:
            print("*** Create ibc_init.h failed! ***")
            self.ibc_init_h = 0
            return -1

        self.ibc_init_h = 1
        pass
    def GenerateAllFile(self, debug=False):
        if self.c_ast is None:
            print("Not init this object with AST!")
            return 0
        try:
            self.GenarateMsgH(debug=debug)
            self.GenerateMsgFuncH(debug=debug)
            self.GenerateMsgC(debug=debug)
            self.GenerateExampleC(debug=debug)
            self.GenerateExampleH(debug=debug)
            self.GenerateDescripteH(debug=debug)
            self.GenerateIbcInitC(debug=debug)
        except Exception:
            print("Call GenerateAllFile failed!")
            pass
        pass

    def Status(self):
        if self.c_ast is None:
            print("Not init this object with AST!")
            return 0

        if self.msg_h: print("1. msg.h created!")
        if self.msg_func_h: print("2. msg_func.h created!")
        if self.msg_c: print("3. msg.c created!")
        if self.example_c: print("4. example.c created!")
        if self.example_h: print("5. example.h created!")
        if self.descripte_h: print("6. descripte.h created!")
        if self.ibc_init_h: print("7. descripte.h created!")
        pass

    pass


def AutoCoderGernerateAllIbcFiles():
    Parser = CHeaderParser(debug=False)
    HeaderAST = Parser.ParseFile("./tmp/struct.h")
    builer = AutoCoder(FileAST=HeaderAST)
    builer.GenerateAllFile()
    builer.Status()
    #vit = TypedefVisitor()
    #vit.visit(HeaderAST)
    pass


if __name__ == '__main__':
    AutoCoderGernerateAllIbcFiles()
