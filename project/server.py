#!/usr/bin/python
import time
import asyncio
import datetime
import sys

loop = asyncio.get_event_loop()
connected_servers = []
user_data = {}
# user_name : [server,skew,lat,lon,time]

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

def handle_latlon(lat_lon_str):
    print(lat_lon_str)
    if(lat_lon_str[0] == "+"):
        lat_lon = lat_lon_str.strip("+")
        print(lat_lon)
        pos_split = lat_lon.split("+")
        neg_split = lat_lon.split("-")
        if(len(pos_split) == 2):
            lat = float(pos_split[0])
            lon = float(pos_split[1])
        elif(len(neg_split) == 2):
            lat = float(neg_split[0])
            lon = float("-{0}".format(neg_split[1]))
        else:
            raise ValueError("invalid input")                                  
    elif(lat_lon_str[0] == "-"):
        lat_lon = lat_lon_str.strip("-")
        pos_split = lat_lon.split("+")
        neg_split = lat_lon.split("-")
        if(len(pos_split) == 2):
            lat = float("-{0}".format(pos_split[0]))
            lon = float(pos_split[1])
        elif(len(neg_split) == 2):
            lat = float("-{0}".format(neg_split[0]))
            lon = float("-{0}".format(neg_split[1]))
        else:
            raise ValueError("invalid input")         
    else:              
        raise ValueError("invalid input")
    return lat, lon


async def handle_iamat(message, writer, received_time):
    try:
        if len(message) == 4:
            lat, lon = handle_latlon(message[2])
            print(lat)
            print(lon)
            skew = received_time - float(message[3])
            skew_str = "+{0}".format(skew) if skew >= 0 else "{0}".format(skew)
            response = "AT {0} {1} {2}\n".format(sys.argv[1],skew_str, " ".join(message[1:]))   
            user_data[message[1]] = [sys.argv[1], skew_str, lat, lon, message[3]]
            writer.write(response.encode())
            return 0
        else:          
            return 1
    except Exception as e:
        print(e)        
        return 1

async def handle_whatsat(message, writer):
    try:
        if len(message) == 4:
            curr_user = user_data[message[1]]

            lat = "+{0}".format(curr_user[2]) if curr_user[2] >= 0 else "{0}".format(curr_user[2])
            lon = "+{0}".format(curr_user[3]) if curr_user[3] >= 0 else "{0}".format(curr_user[3])
            lat_lon = "{0}{1}".format(lat,lon)

            response = "AT {0} {1} {2} {3} {4}\n".format(curr_user[0],curr_user[1], message[1], lat_lon, curr_user[4])           
            writer.write(response.encode())
            return 0
        else:
            return 1
    except:
        return 1


async def server_routine(reader, writer):
    while True:
        try:
            received_time = time.time()
            received = await reader.readuntil(b'\n')
            received_decoded = received.decode().strip("\n").strip("\r")
            received_decomp = received_decoded.split()
            print('Received: %r' % received_decoded)
            error = 0
            if (len(received_decomp) > 0):
                if (received_decomp[0] == "IAMAT"):
                    error = await handle_iamat(received_decomp, writer, received_time)
                elif (received_decomp[0] == "WHATSAT"):
                    error = await handle_whatsat(received_decomp, writer)
                elif (received_decomp[0] == "AT"):
                    await propagate_message(received_decoded)
                else:
                    error = 1
            else:
                error = 1
            if error:
                writer.write("? {0}".format(received.decode()).encode())
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