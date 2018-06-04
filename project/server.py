#!/usr/bin/python

import asyncio
import datetime
import sys

def get_port_num(server_name):
    return {
        'Goloman' : 11989,
        'Hands' : 11990,
        'Holiday' : 11991,
        'Wilkes' : 11992,
        'Welsh' : 11993
    } [server_name]

def talks_with(server_name):
    return {
        'Goloman' : ['Hands', 'Holiday', 'Wilkes'],
        'Hands' : ['Goloman', 'Wilkes'],
        'Holiday' : ['Goloman', 'Welsh', 'Wilkes'],
        'Wilkes': ['Goloman', 'Hands', 'Holiday'],
        'Welsh' : ['Holiday']
    } [server_name]


async def server_routine(reader, writer):
    print("hello")
    while True :
        received = await reader.readuntil(b'\n')
        if received == b'':
            break
        message = received.decode()
        addr = writer.get_extra_info('peername')
        print("Received %r from %r" % (message, addr))
        writer.write("received {}".format(received.decode()).encode())

    writer.write("hello".encode())
    await writer.drain()

async def connection_routine(servers, loop):
    reader, writer = await asyncio.open_connection('127.0.0.1', get_port_num(servers[0]), loop=loop)
    message = input("give me something to say: ")
    writer.write(message.encode)
    data = await reader.read(100)
    print('Received: %r' % data.decode())


def main():
    server_port = get_port_num(sys.argv[1])
    loop = asyncio.get_event_loop()
    routine = asyncio.start_server(server_routine, '127.0.0.1', server_port, loop=loop)
    server = loop.run_until_complete(routine)

    loop.run_until_complete(connection_routine(talks_with(sys.argv[1]), loop))
    #routine2 = asyncio.start_server(server_routine, '127.0.0.1', get_port_num('Hands'), loop=loop)
    #server2 = loop.run_until_complete(routine2)

    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass
    
    server.close()
    #server2.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()



if __name__ == '__main__':
    if len(sys.argv) == 2:
        if sys.argv[1] in ['Goloman','Hands','Holiday','Wilkes','Welsh']:
            main()
        else:
            print("Invalid Server Name : {0}".format(sys.argv[1]))
    else:
        print("Invalid arguments")