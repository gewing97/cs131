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


async def server_routine(reader, writer):
    #print(writer.get_extra_info("socket"))
    #print(writer.get_extra_info("peername"))
    #print(writer.get_extra_info("sockname")[1])
    while True:
        try:
            received = await reader.readuntil(b'\n')
            received_decoded = received.decode().strip("\n").strip("\r")
            print('Received: %r' % received_decoded)
            message_to_friends = "{0} heard {1}".format(sys.argv[1], received.decode())
            for friend in talks_with(sys.argv[1]):
                friend_port = get_port_num(friend)
                if friend != received_decoded.split(" ")[0]:
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
                            connected_servers.append(await connection_routine(friend_port, message_to_friends,loop))
                        except:
                            pass
        except:
            break
    writer.close()

async def connection_routine(server_port, message, loop):
    reader, writer = await asyncio.open_connection('127.0.0.1', server_port, loop=loop)
    print('%r' % message)        
    writer.write(message.encode())
    await writer.drain()        
    return server_port, reader, writer

async def connection_generator(servers, server_name, loop):
    try:
        await asyncio.ensure_future(connection_routine(get_port_num(servers[0]), server_name, loop))
    except:
        pass


def main():
    server_port = get_port_num(sys.argv[1])
    routine = asyncio.start_server(server_routine, '127.0.0.1', server_port, loop=loop)
    server = loop.run_until_complete(routine)

    #connection = loop.run_until_complete(connection_generator(talks_with(sys.argv[1]), sys.argv[1], loop))
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