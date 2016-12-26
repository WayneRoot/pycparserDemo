typedef unsigned int ip_t;
typedef unsigned char ip6_t[16];
typedef unsigned short vlan_t;
typedef unsigned short ethertype_t;
typedef int l4_port_t;
typedef unsigned char uint8;
typedef unsigned int uint32;
typedef unsigned char mac_t[6];

typedef struct a {
	int a_a;
	unsigned int a_b;
	char * a_c;
} struct_a;

typedef struct b {
	unsigned char  b_a[128];
} struct_a;

int slot_hello1(int rv);

int slot_hello2(int rv, int rv1);

int slot_hello3(int rv, int rv2, char rv3);