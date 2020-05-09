"""
Note that this piece of code is (of course) only a hint
you are not required to use it
neither do you have to use any of the methods mentioned here
The code comes from
https://asyncio.readthedocs.io/en/latest/tcp_echo.html
To run:
1. start the echo_server.py first in a terminal
2. start the echo_client.py in another terminal
3. follow print-back instructions on client side until you quit
"""

import asyncio
import argparse
import time
import json
import sys
import re
import aiohttp
import logging

API_KEY = 'AIzaSyBgF5eIOsAbx3MSVicCeszCxuTpB1-HPA4'
#rVal = ""
pattern = "(\+|\-)(\d+)(\.)(\d+)(\-)(\d{3})(\.)(\d+)"
pattern2 = "(\d+)(\.)(\d+)"
switchR = {
    'Hill':['Jaquez', 'Smith'],
    'Singleton':['Jaquez', 'Smith', 'Campbell'],
    'Smith' : ['Campbell'],
    'Jaquez' : [],
    'Campbell' : []    
    }

switcher2 = {
    'Hill':12310,
    'Jaquez':12311,
    'Smith':12312,
    'Campbell':12313,
    'Singleton':12314
}


history = dict()

async def flooding2(message, name):
    for s in switchR[name]:
        try:
            reader, writer =  await asyncio.open_connection('127.0.0.1', switcher2[s], loop=loop)
            writer.write(message.encode())
            await writer.drain()
            writer.write_eof()
            writer.close()
        except:
            print("ERROR")
         

class Server(asyncio.Protocol):
    #history = dict()
    def __init__(self, name, ip='127.0.0.1', port=888, message_max_length=1e6):
        switcher = {
            'Hill':12310,
            'Jaquez':12311,
            'Smith':12312,
            'Campbell':12313,
            'Singleton':12314
            }
        self.name = name
        self.ip = ip
        self.port = switcher.get(name, "Invalid port number")
        self.message_max_length = int(message_max_length)

    def validIAMAT(self, message):
        if re.match(pattern, message[2]) == None:
            return False
        if re.match(pattern2, message[3]) == None:
            return False
        return True

    def gettingCoord(self, c):
        coords = re.split('[+-]', c[1:])
        return coords[0], float(coords[1])*(-1)
    
    async def flooding(self, message):
        for s in switchR[self.name]:
            try:
                reader, writer =  await asyncio.open_connection('127.0.0.1', self.switcher[s], loop=loop)
                writer.write(message.encode())
                await writer.drain()
                writer.write_eof()
                writer.close()
            except:
                print("ERROR")
                #log_file.write("Cannot connect to the server {}\n".format(self.name))
        
    def bounds(self, message):
        client_id = message[1]
        radius = int(message[2])
        upper = int(message[3])
        if radius < 0 or radius > 50:
            return False
        if upper < 0 or upper > 20:
            return False
        if message[1] not in history.keys():
            print("PRINT NOT IN HISTORY")
            print(history)
            print(message)
            return False
        return True

    async def checkWHATSAT(self, message):
        print(history)
        client_id = message[1]
        radius = int(message[2])
        upper = int(message[3])
        radius2 = str(radius * 1000) 
        if self.bounds(message) == False:
            return "? {}\n".format(message)
        diffTime = time.time() - float(history[message[1]][3])
        if diffTime > 0:
            diffTime = "+" + str(diffTime)
        msg = f"AT {history[message[1]][5]} {diffTime} {client_id} {history[message[1]][2]} {history[message[1]][3]}"
        print(msg)
                
        latitude, longitude = (self.gettingCoord(history[message[1]][2]))
        print(self.gettingCoord(history[message[1]][2]))

        loc = "{0},{1}".format(latitude, longitude)
        url = 'https://maps.googleapis.com/maps/api/place/nearbysearch/json?key={0}&location={1}&radius={2}'.format(API_KEY, loc, radius2)

        async with aiohttp.ClientSession() as session:
            async with session.get(url) as resp:
                response = await resp.json()

        response['results'] = response['results'][:upper]
        #print(response)
        return msg + '\n' + json.dumps(response, indent=4) + '\n'

    async def notFlooding(self, message, response, reader, writer):
        if(message[0] != "Flooding"):
            writer.write(response.encode())
            await writer.drain()
            writer.write_eof()
            writer.close()

    
    def checkParams(self, message):
        if message[0] == "IAMAT" and len(message) != 4:
            return False
        if message[0] == "WHATSAT" and len(message) != 4:
            return False
        if message[0] == "Flooding" and len(message) != 6:
            return False
        return True

    def callLogging(self, message, addr):
        logging.info("{} received {} from {}".format(self.name, message, addr))

    async def handle_echo(self, reader, writer):
        """
        on server side
        """
        data = await reader.read(self.message_max_length)
        message1 = data.decode()
        addr = writer.get_extra_info('peername')
        #logging.info("{} received {} from {}".format(self.name, message, addr))
        self.callLogging(message1, addr)
        sendback_message = message1.strip()
        messageArr = sendback_message.split()
        x = 1
        if self.checkParams(messageArr) == False:
            response = ("? {}\n".format(messageArr))
        else:
            if messageArr[0] == "IAMAT":
                message = messageArr
                if (self.validIAMAT(message)) == None:
                    response = "? {}\n".format(message)
                client_id = message[1]
                timestamp = message[3]
                coordinates = message[2]
                message2 = message + [str(timestamp), self.name]
                diffTime = time.time() - float(timestamp)
                if diffTime > 0:
                    diffTime = "+" + str(diffTime)
                global history
                history[client_id] = message2
                print("IAMAT")
                print(history)
                msg = f"AT {self.name} +{diffTime} {client_id} {coordinates} {timestamp}"
                asyncio.create_task(flooding2("Flooding " + ' '.join(message2[1:]), self.name))
                response =  msg
            elif messageArr[0] == "WHATSAT":
                response = await self.checkWHATSAT(messageArr)

            elif messageArr[0] == "Flooding":
                message = messageArr
                if message[1] not in history or history[message[1]][3] < message[3]:
                    history[message[1]] = message
                    asyncio.create_task(self.flooding(' '.join(message)))
            else:
                response =  ("? {}\n".format(messageArr))

        await self.notFlooding(messageArr, response, reader, writer)
        
    async def run_forever(self):
        server = await asyncio.start_server(self.handle_echo, self.ip, self.port)

    # Serve requests until Ctrl+C is pressed
        print(f'serving on {server.sockets[0].getsockname()}')
        async with server:
            await server.serve_forever()
    # Close the server
        server.close()
        

def main():
    parser = argparse.ArgumentParser('CS131 project example argument parser')
    parser.add_argument('server_name', type=str,
                        help='required server name input')
    args = parser.parse_args()

    logging.basicConfig(filename='{}.log'.format(args.server_name), level=logging.INFO)
    logging.info("Hello, welcome to server {}".format(args.server_name))
    print("Hello, welcome to server {}".format(args.server_name))
    server = Server(args.server_name)

    
    global loop
    loop = asyncio.get_event_loop()
    coro = asyncio.start_server(server.handle_echo, '127.0.0.1', switcher2[args.server_name], loop=loop)
    server2 = loop.run_until_complete(coro)

    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass

    server2.close()
    loop.run_until_complete(server2.wait_closed())
    loop.close()
    #log_file.close()
    
    """try:
        asyncio.run(server.run_forever())
    except KeyboardInterrupt:
        pass"""

if __name__ == '__main__':
    main()
