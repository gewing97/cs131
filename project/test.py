import asyncio


async def tcp_echo_client(loop):
    reader, writer = await asyncio.open_connection('127.0.0.1', 8888, loop=loop)
    for i in range(5):
        message = input("give me something to say: ")
        print('Send: %r %d' % (message,i))
        writer.write(message.encode())

        data = await reader.read(100)
        print('Received: %r' % data.decode())

    writer.write("close".encode())
    print('Close the socket')
    writer.close()

loop = asyncio.get_event_loop()
loop.run_until_complete(tcp_echo_client(loop))
loop.close()