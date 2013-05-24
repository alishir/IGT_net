IOPort('CloseAll');
resp_config = 'BaudRate=9600 StopBits=0 Parity=None DataBits=8';
[resp_handle, errmsg] = IOPort('OpenSerialPort', 'COM4', resp_config);
IOPort('Purge', resp_handle);
resp_data = 1;
triger_pre = 0;
count = 0;
date_dummy = 0;
while(1)
    if (IOPort('BytesAvailable', resp_handle))
        [resp_data, when, errmsg] = IOPort('Read', resp_handle, 1, 1);
        n = IOPort('BytesAvailable', resp_handle);
        [date_dummy, when, errmsg] = IOPort('Read', resp_handle, 1, n);
        fprintf('Resp: %d\n', resp_data);
        resp_data = -1;
    end
    %while(IOPort('BytesAvailable', resp_handle))
        
    %end
end