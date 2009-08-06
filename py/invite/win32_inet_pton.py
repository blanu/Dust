# This should really be in Python

import socket
import struct
import ctypes
import ctypes.wintypes

SOCKET_ERROR = -1

ADDRESS_FAMILY = ctypes.c_ushort

_SS_MAXSIZE = 128
_SS_ALIGNSIZE = ctypes.sizeof(ctypes.c_int64)

_SS_PAD1SIZE = _SS_ALIGNSIZE - ctypes.sizeof(ctypes.c_ushort)
_SS_PAD2SIZE = _SS_MAXSIZE - (ctypes.sizeof(ctypes.c_ushort) + _SS_PAD1SIZE + _SS_ALIGNSIZE)

class in_addr(ctypes.Structure):
    _fields_ = [ ("S_addr", ctypes.c_ulong) ]

class in6_addr(ctypes.Structure):
    _fields_ = [ ("Byte", ctypes.c_char * 16) ]

class sockaddr_storage(ctypes.Structure):
    _fields_ = [ ("ss_family", ADDRESS_FAMILY),
                 ("__ss_pad1", ctypes.c_char * _SS_PAD1SIZE),
                 ("__ss_align", ctypes.c_int64),
                 ("__ss_pad2", ctypes.c_char * _SS_PAD2SIZE) ]

class sockaddr_in(ctypes.Structure):
    _fields_ = [ ("sin_family", ADDRESS_FAMILY),
                 ("sin_port", ctypes.c_ushort),
                 ("sin_addr", in_addr),
                 ("sin_zero", ctypes.c_char * 8) ]
LP_sockaddr_in = ctypes.POINTER(sockaddr_in)

class sockaddr_in6(ctypes.Structure):
    _fields_ = [ ("sin6_family", ctypes.c_short),
                 ("sin6_port", ctypes.c_ushort),
                 ("sin6_flowinfo", ctypes.c_ulong),
                 ("sin6_addr", in6_addr),
                 ("sin6_scope_id", ctypes.c_ulong) ]
LP_sockaddr_in6 = ctypes.POINTER(sockaddr_in6)


def inet_pton(af, src):
    dst = sockaddr_storage()
    dstlen = ctypes.c_int(ctypes.sizeof(dst))
    r = ctypes.windll.ws2_32.WSAStringToAddressA(src.encode("ascii"), af, 0,
                                                 ctypes.byref(dst),
                                                 ctypes.byref(dstlen))
    if r == SOCKET_ERROR:
        raise ctypes.WinError()
    pdst = ctypes.pointer(dst)
    if af == socket.AF_INET:
        addr = ctypes.cast(pdst, LP_sockaddr_in).contents.sin_addr
    else:
        addr = ctypes.cast(pdst, LP_sockaddr_in6).contents.sin6_addr
    return ctypes.string_at(ctypes.addressof(addr), ctypes.sizeof(addr))

def inet_ntop(af, src):
    ss = sockaddr_storage()
    pss = ctypes.pointer(ss)
    if af == socket.AF_INET:
        srclen = ctypes.sizeof(sockaddr_in)
        sa = ctypes.cast(pss, LP_sockaddr_in).contents
        sa.sin_family = socket.AF_INET
        sa.sin_port = 0
        ctypes.memmove(ctypes.pointer(sa.sin_addr), src,
                       ctypes.sizeof(sa.sin_addr))
    else:
        srclen = ctypes.sizeof(sockaddr_in6)
        sa = ctypes.cast(pss, LP_sockaddr_in6).contents
        sa.sin6_family = socket.AF_INET6
        sa.sin6_port = 0
        sa.sin6_flowinfo = 0
        ctypes.memmove(ctypes.pointer(sa.sin6_addr), src,
                       ctypes.sizeof(sa.sin6_addr))

    dst = ctypes.create_string_buffer(40)
    dstlen = ctypes.wintypes.DWORD(len(dst))
    r = ctypes.windll.ws2_32.WSAAddressToStringA(ctypes.byref(ss), srclen, 0,
                                                 ctypes.byref(dst),
                                                 ctypes.byref(dstlen))
    if r == SOCKET_ERROR:
        raise ctypes.WinError()
    return ctypes.string_at(ctypes.addressof(dst), dstlen.value-1).decode("ascii")

def test(af, ip):
    b = inet_pton(af, ip)
    r = inet_ntop(af, b)
    assert(r == ip)

test(socket.AF_INET, "127.0.0.1")
test(socket.AF_INET6, "::1")
