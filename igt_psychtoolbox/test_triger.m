IOPort('CloseAll');
triger_config = 'BaudRate=4800 StopBits=0 Parity=None DataBits=8';
[triger_handle, errmsg] = IOPort('OpenSerialPort', 'COM1', triger_config);
IOPort('Purge', triger_handle);
triger_data = 1;
triger_pre = 0;
count = 0;
date_dummy = 0;
while(1)
     while (triger_data ~= 48)
         if (IOPort('BytesAvailable', triger_handle))
            [triger_data, when, errmsg] = IOPort('Read', triger_handle, 1, 1);
         end
        while(IOPort('BytesAvailable', triger_handle))
            [date_dummy, when, errmsg] = IOPort('Read', triger_handle, 1, 1);
        end
     end
    fprintf('Trigger: %d -- count: %d\n', triger_data, count);
    count = count + 1;
    triger_data = 0;
     
end