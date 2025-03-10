with Ada.Text_IO;
with Ada.Command_Line;
with Ada.Streams;
with GNAT.Serial_Communications;

procedure UART_Receiver is

   use Ada.Text_IO;
   use Ada.Streams;
   use GNAT.Serial_Communications;

   --  Default serial port settings
   Port_Device : String (1 .. 32) := (others => ' ');  --  Buffer for port name
   Actual_Length : Natural := 0;  --  Actual length of the port name
   Baud_Rate   : Data_Rate := B115200;
   Port        : Serial_Port;

   --  Buffer for receiving data
   Buffer_Size : constant := 1024;
   Buffer      : Stream_Element_Array (1 .. Buffer_Size);
   Last        : Stream_Element_Offset;

   --  Set the port name in our fixed buffer
   procedure Set_Port_Name (Name : String) is
   begin
      Port_Device := (others => ' ');  --  Clear the buffer
      Actual_Length := Natural'Min (Name'Length, Port_Device'Length);
      Port_Device (1 .. Actual_Length) := Name (Name'First .. Name'First + Actual_Length - 1);
   end Set_Port_Name;

begin
   --  Default port names
   if On_Windows then
      Set_Port_Name ("COM3");  --  Common port for XDS110 debug probe on Windows
   else
      Set_Port_Name ("/dev/ttyACM0");  --  Default for Linux
   end if;

   --  Check if port name was provided as command line argument
   if Ada.Command_Line.Argument_Count >= 1 then
      Set_Port_Name (Ada.Command_Line.Argument (1));
   end if;

   Put_Line ("UART Receiver for TI IWR1843BOOST");
   Put_Line ("--------------------------------");
   Put_Line ("Opening serial port: " & Port_Device (1 .. Actual_Length));

   --  Open the serial port
   begin
      Open (Port, Name => Port_Device (1 .. Actual_Length));
      Set (Port, Rate => Baud_Rate, 
           Bits => Data_Bits_8, 
           Stop_Bits => Stop_Bits_1, 
           Parity => No_Parity, 
           Block => False);
      Put_Line ("Serial port opened successfully");
   exception
      when others =>
         Put_Line ("Error: Failed to open serial port " & 
                  Port_Device (1 .. Actual_Length));
         Put_Line ("Please provide a valid port name as an argument");
         return;
   end;

   Put_Line ("Waiting for data from TI IWR1843BOOST...");
   Put_Line ("Press Ctrl+C to exit");

   --  Main loop - continuously read and display data
   loop
      --  Read data from serial port
      begin
         Read (Port, Buffer, Last);

         --  Only process if we received data
         if Last >= Buffer'First then
            --  Convert to string for display
            declare
               Data : String (1 .. Integer (Last));
            begin
               for I in 1 .. Last loop
                  Data (Integer (I)) := Character'Val (Buffer (I));
               end loop;

               --  Print the received data
               Put (Data);
            end;
         end if;

         --  Small delay to prevent busy-waiting
         delay 0.01;
      exception
         when others =>
            Put_Line ("Error reading from port");
            exit;
      end;
   end loop;

   --  Close the serial port
   Close (Port);

exception
   when others =>
      Put_Line ("Unhandled exception occurred");
      if Is_Open (Port) then
         Close (Port);
      end if;
end UART_Receiver;