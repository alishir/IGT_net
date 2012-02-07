#include "sender.h"

Sender::Sender()
    :QThread(0)
{

}

void Sender::run()
{
    this->connections = new QList<QTcpSocket*>();
    this->image = new QImage();
    this->image->load("test.png", "PNG");
    this->ba = new QByteArray;
    this->buff = new QBuffer(this->ba);
    this->image->save(this->buff, "PNG");

    this->server = new QTcpServer();
    connect(this->server, SIGNAL(newConnection()), this, SLOT(handleNewConnection()));
    this->server->listen(QHostAddress::Any, 12345);
    qDebug() << "Start Listening on port 12345";
    exec();
}

void Sender::handleNewConnection()
{
    QTcpSocket *t = this->server->nextPendingConnection();
    this->connections->append(t);
    qDebug() << "New Connection Established :D";
    this->sendImage(t);
}

void Sender::sendImage(QTcpSocket *t)
{
    qDebug() << "Sending Image ... Size: " << this->ba->size();
    t->write(*(this->ba), this->ba->size());
    t->flush();
}
