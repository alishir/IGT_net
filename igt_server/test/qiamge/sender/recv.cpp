#include "recv.h"
#include <QBuffer>
#include <QImage>
#include <QByteArray>

Recv::Recv(int id)
    :QThread(0)
    ,id(id)
{
}

Recv::~Recv()
{
    qDebug() << "Recv id: " << id << "Dead!";
}

void Recv::run()
{
    this->connection = new QTcpSocket();
    connect(this->connection, SIGNAL(connected()), this, SLOT(connected()));
    connect(this->connection, SIGNAL(readyRead()), this, SLOT(readImage()));
    this->connection->connectToHost("127.0.0.1", 12345);
    exec();
}

void Recv::readImage()
{
    QByteArray *ba = new QByteArray();
    QByteArray data = this->connection->readAll();
    QBuffer *buff = new QBuffer(ba);
    buff->setData(data.data(), data.size());

    qDebug() << "Bytes:" << buff->size();
    QImage *image = new QImage();
    image->loadFromData(buff->buffer());
    QString fn("client");
    fn.append(QString::number(this->id, 10));
    image->save(fn, "PNG");
    delete buff;
    delete image;
//    this->terminate();
}

void Recv::connected()
{
    qDebug() << "Recv id: " << id << " is connected to server :P";
}

