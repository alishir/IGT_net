#ifndef SENDER_H
#define SENDER_H

#include <QThread>
#include <QImage>
#include <QTcpSocket>
#include <QTcpServer>
#include <QByteArray>
#include <QBuffer>


class Sender : public QThread
{
    Q_OBJECT
public:
    Sender();
protected:
    void run();

protected slots:
    void handleNewConnection();

private:
    void sendImage(QTcpSocket *t);
    QTcpServer *server;
    QList<QTcpSocket*> *connections;
    QImage* image;
    QByteArray *ba;
    QBuffer *buff;
};

#endif // SENDER_H
