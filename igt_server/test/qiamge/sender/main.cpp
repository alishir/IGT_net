#include <QtCore/QCoreApplication>
#include <sender.h>
#include <recv.h>
#include <unistd.h>

int main(int argc, char *argv[])
{
    QCoreApplication a(argc, argv);

    Sender *srv = new Sender();
    srv->start();
    sleep(5);
    Recv *recv1 = new Recv(1);
    Recv *recv2 = new Recv(2);
    Recv *recv3 = new Recv(3);
    Recv *recv4 = new Recv(4);
    Recv *recv5 = new Recv(5);
    qDebug() << "Going to Start Recvs";
    recv1->start();
    recv2->start();
    recv3->start();
    recv4->start();
    recv5->start();

    return a.exec();
}
