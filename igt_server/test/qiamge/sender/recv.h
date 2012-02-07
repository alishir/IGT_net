#ifndef RECV_H
#define RECV_H

#include <QThread>
#include <QTcpSocket>

class Recv : public QThread
{
    Q_OBJECT
public:
    Recv(int id);
    ~Recv();
protected:
    void run();

protected slots:
    void connected();
    void readImage();

private:
    int id;
    QTcpSocket *connection;
};

#endif // RECV_H
