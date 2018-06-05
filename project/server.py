#!/usr/bin/python

import asyncio
import datetime
import sys

loop = asyncio.get_event_loop()
connected_servers = []

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

async def propagate_message(message):
    composition = message.split(" ")
    message_to_friends = "{0} {1}\n".format(message, sys.argv[1])
    if len(composition) > 6:
        up_stream = composition[6:]
    else:
        up_stream = []
    for friend in talks_with(sys.argv[1]):
        friend_port = get_port_num(friend)
        if friend not in up_stream:
            persistent_connection = False
            for connected in connected_servers:
                if friend_port == connected[0]:
                    try:
                        connected[2].write(message_to_friends.encode())
                        await connected[2].drain()
                        persistent_connection = True                        
                    except Exception as e:
                        print(e)
                        connected_servers.remove(connected)
                        persistent_connection = False
            if not persistent_connection:
                try:
                    connected_servers.append(await connection_routine(friend_port, message_to_friends))
                except:
                    pass

async def maintain_connections(up_stream_friends):
    ports = []
    for i in up_stream_friends:
        ports.append(get_port_num(i))
    for connected in connected_servers:
        if connected[0] in ports:
            ports.remove(connected[0])
    for maintain in ports:
        try:
            reader, writer = await asyncio.open_connection('127.0.0.1', maintain, loop=loop)
            connected_servers.append(maintain, reader, writer)        
        except:
            pass

async def connection_routine(server_port, message):
    reader, writer = await asyncio.open_connection('127.0.0.1', server_port, loop=loop)   
    writer.write(message.encode())
    await writer.drain()        
    return server_port, reader, writer


#def handle_iamat(message, writer):

async def server_routine(reader, writer):
    while True:
        try:
            received = await reader.readuntil(b'\n')
            received_decoded = received.decode().strip("\n").strip("\r")
            print('Received: %r' % received_decoded)
            await propagate_message(received_decoded)
        except:
            break


def main():
    server_port = get_port_num(sys.argv[1])
    routine = asyncio.start_server(server_routine, '127.0.0.1', server_port, loop=loop)
    server = loop.run_until_complete(routine)

    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass

    server.close()
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