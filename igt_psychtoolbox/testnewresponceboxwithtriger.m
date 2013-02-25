config_display( 0, 2, [0 0 0], [1 0.5 0.75], 'Segoe Script', 80, 4 )


config_data( 'test3.dat' );
config_log( );
% ReacT=zeros(2,100);
% ReAcT=[];

%responsebox initialization
s=serial('COM4');% use device manager to findout com number of usbserial port after pluging the response box
s.inputbuffersize=1;
fopen(s);
s.timeout=0.001; %time out is the time that matlab wait for data

%end of initialization of response box

config_serial(1,4800,0,0,8)
start_cogent;




%  preparestring( '--------', 2 ); % Draw fixation point in display buffer 2
W_t=1000;
trial=2;
for j=1:trial
        for i = 1:countdatarows

           word = getdata( i, 1 );    
           clearpict( 1 );
           preparestring( word, 1 ); % Draw word in display buffer 1
 
           %**********WAIT FOR EPI TRIGER====
           clearserialbytes(1);
           waitserialbyte(1,inf,48);
           readserialbytes(1);
   
           %============================


           t0 = drawpict( 1 ); % Display word and get time

           str=sprintf('%s: %d',word,t0);    % Log word and time it was displayed
           logstring(str);
%            RT=[];
            m = 0;
           kd=0;kp=0;

           TQ=1500;  %assumed TR is 2 sec
         
          if (s.BytesAvailable~=0)
              kp=fread(s);
          end
          tic
          t=0;
          while(1000*toc < TQ) 
               if (s.BytesAvailable~=0)
                    kp=fread(s);

                      m = kp (1);

                      rt=1000*toc;
                      
                     str=sprintf('%s %d %s %8.4f','Key no ',m,'is pressed and reaction time was:', rt-t);
                     logstring(str);
                     t=rt;
                     
                   end
              m=0;      
          end
   
    end
% ReacT(i,1:length(RT))=RT;
end 
% ReAcT=[ReAcT;ReacT];
 
stop_cogent;
 fclose(s);