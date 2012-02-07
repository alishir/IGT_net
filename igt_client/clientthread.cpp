#include <string.h>
#include "clientthread.h"
#include "mainwindow.h"

ClientThread::ClientThread(QString serverIP, int port, QObject* parent)
    :QThread(parent)
    ,parent(parent)
    ,tcpConnection(0)
    ,serverAddress(serverIP)
    ,serverPort(port)
    ,ba(0)
    ,buff(0)
    ,imageData(0)
    ,imageDataSize(0)
{
}

ClientThread::~ClientThread()
{
    this->tcpConnection->close();
    delete this->tcpConnection;
    delete this->ba;
    delete this->buff;
    delete this->imageData;
}

void ClientThread::run()
{
    if (this->buff == 0)
    {
        this->ba = new QByteArray();
        this->buff = new QBuffer(this->ba);
        this->imageData = new QByteArray();
    }

    if (this->tcpConnection == 0)
    {
        this->tcpConnection = new QTcpSocket(this);
        connect(this->tcpConnection, SIGNAL(connected()), this, SLOT(connected()));
        connect(this->tcpConnection, SIGNAL(disconnected()), this, SLOT(disconnected()));
        connect(this->tcpConnection, SIGNAL(readyRead()), this, SLOT(getImage()));
        this->tcpConnection->connectToHost(this->serverAddress, this->serverPort);
    }
    exec();
}

void ClientThread::connected()
{
    qDebug() << "Connection to Server ";
}

void ClientThread::disconnected()
{
    qDebug() << "Disconnceted from Server ";
}

void ClientThread::getImage()
{
    bool stat;
    bool sizeErr = false;
    int bytes;
    QImage image;
    int headerSize = 9;
    while(this->tcpConnection->bytesAvailable())
    {
        if ((this->imageDataSize == 0) && (this->imageData->size() >= headerSize))
        {
            qDebug() << "man injam cache:" << this->imageData->left(headerSize - sizeof(int));
            if (QString::compare("size:", this->imageData->left(headerSize - sizeof(int))) == 0)
            {
                this->imageData->remove(0, headerSize - sizeof(int));
                this->imageDataSize = *((int *) (this->imageData->left(sizeof(int)).data()));
                qDebug() << "image size: " << this->imageDataSize;
                this->imageData->remove(0, sizeof(int));
                this->imageData->append(this->tcpConnection->readAll());
            }
            else
            {
                sizeErr = true;
            }
        }
        else if ((this->imageDataSize == 0) && (this->tcpConnection->bytesAvailable() >= headerSize))
        {
            this->imageData->append(this->tcpConnection->readAll());
            qDebug() << "man injam:" << this->imageData->left(headerSize - sizeof(int));
            if (QString::compare("size:", this->imageData->left(headerSize - sizeof(int))) == 0)
            {
                this->imageData->remove(0, headerSize - sizeof(int));
                this->imageDataSize = *((int *) (this->imageData->left(sizeof(int)).data()));
                qDebug() << "image size: " << this->imageDataSize;
                this->imageData->remove(0, sizeof(int));
            }
            else
            {
                sizeErr = true;
            }
        }
        else if (this->tcpConnection->bytesAvailable() >= (this->imageDataSize - this->imageData->size()))
        {
            qDebug() << "over doze :D";
            this->imageData->append(this->tcpConnection->readAll());
        }


        if (sizeErr)
        {
            int err = 0;
            do
            {
                this->imageData->append(this->tcpConnection->readAll());
                err = this->imageData->lastIndexOf("size:");
                this->imageData->remove(0, err);
                qDebug() << "some error in cache, index size: " <<  err;
            }
            while(err == -1);
            this->imageDataSize = 0;
            sizeErr = false;
        }
        else if (this->imageData->size() > 2 * this->imageDataSize)
        {
            this->imageData->append(this->tcpConnection->readAll());
            int last = this->imageData->lastIndexOf("size:");
            this->imageData->remove(0, last);
            this->imageDataSize = 0;
        }
        else if (this->imageData->size() >= this->imageDataSize)
        {
            this->buff->open(QIODevice::ReadWrite);

            bytes = this->buff->write(this->imageData->left(this->imageDataSize));
            if (bytes == -1)
            {
                qDebug() << "Buffer write error ...";
            }
            else if (bytes < this->imageDataSize)
            {
                qDebug() << "Less data has been writtern";
            }
            this->imageData->remove(0, this->imageDataSize);
            stat = image.loadFromData(this->buff->buffer());
            this->imageDataSize = 0;
            this->buff->close();
            if (!stat)
            {
                qDebug() << "Buff -- Some Error in loading image";
                this->imageData->clear();
                this->buff->buffer().clear();
                this->tcpConnection->readAll();
                this->imageDataSize = 0;
            }
            else
            {
                emit this->updateImage(QPixmap::fromImage(image));
                qDebug() << "Going to get next imaga";
            }
        }
    } // while data
    return;
}
