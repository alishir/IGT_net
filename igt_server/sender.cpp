#include "sender.h"
#include "cv.h"
#include "highgui.h"
#include <QFile>
#include <iostream>
using namespace std;

Sender::Sender(QMutex *frameMutex, bool *dataReady, QObject *parent)
    :QThread(parent)
    ,frameMutex(frameMutex)
    ,dataReady(dataReady)
    ,qImage(0)
    ,sendNext(true)
{
    cvNamedWindow("sender_window", CV_WINDOW_AUTOSIZE);

}

void Sender::run()
{
    this->camConnections = new QList<QTcpSocket*>();

    this->ba = new QByteArray;
    this->buff = new QBuffer(this->ba);
    this->buff->open(QIODevice::WriteOnly);

    this->camServer = new QTcpServer();
    connect(this->camServer, SIGNAL(newConnection()), this, SLOT(handleNewConnection()));
    this->camServer->listen(QHostAddress::Any, 12345);
    qDebug() << "Start Listening on port 12345 for webcam image";

    //    this->imageWatcher = new QFileSystemWatcher(this);
    //    this->imageWatcher->addPath("/tmp/test.jpg");
    //    connect(this->imageWatcher, SIGNAL(fileChanged(QString)), this, SLOT(sendImageFile(QString)));
    exec();
}

void Sender::sendImageFile(QString fn)
{
    QFile imageFile(fn);
    this->frameMutex->lock();
    if(*(this->dataReady) && !imageFile.open(QIODevice::ReadOnly))
    {
        qDebug() << "Couldn't open file: " << fn;
        return;
    }
    *(this->dataReady) = false;
    this->frameMutex->unlock();

    for (int i = 0; i < this->camConnections->size(); i++)
    {
        QByteArray ba = imageFile.readAll();
        char* data = ba.data();
        int size = ba.size();
        if (size > 0)
        {
            this->camConnections->at(i)->write(data, size);
            this->camConnections->at(i)->flush();
        }
    }
}

void Sender::sendComplete(qint64 bytes)
{
    qDebug() << "Data has been sent with size: " << bytes;
}

void Sender::clientDisconnected()
{
    QObject *emitter = QObject::sender();
    QTcpSocket *t = 0;
    if (emitter->inherits("QTcpSocket"))
    {
        t = static_cast<QTcpSocket *>(emitter);
    }
    if (this->camConnections->contains(t))
    {
        int index = this->camConnections->indexOf(t);
        this->camConnections->removeAt(index);
    }
    qDebug() << "Client Disconnected, IP: " << t->peerAddress();
}

void Sender::handleNewConnection()
{
    QTcpSocket *t = this->camServer->nextPendingConnection();
    QTextStream qout(stdout);
    char response;
    qout << "Connection Request from " << t->peerAddress().toString() << "[Accept or Reject](A/r)?"  << endl;
    scanf("%c\n", &response);
    qout << "Your response: " << response << endl;
    if (response == 'A' || response == 'a')
    {
        this->camConnections->append(t);
        connect(t, SIGNAL(disconnected()), this, SLOT(clientDisconnected()));
        //    connect(t, SIGNAL(bytesWritten(qint64)), this, SLOT(sendComplete(qint64)));
        qDebug() << "New Connection Established :D";
    }
}

void Sender::sendImage()
{
    int size;
    for (int i = 0; i < this->camConnections->size(); i++)
    {
        //        qDebug() << "Sending Image ... Size: " << this->ba->size();
        size = this->ba->size();
//        qDebug() << "size is: " << size;
        this->camConnections->at(i)->write("size:");
        this->camConnections->at(i)->write((char *) &size, sizeof(int));
//        this->camConnections->at(i)->flush();
        this->camConnections->at(i)->write(*(this->ba), this->ba->size());
        this->camConnections->at(i)->flush();
    }
}

void Sender::getFrame(IplImage *frame)
{
    //    qDebug() << "Getting Frame ...";
    if (this->camConnections->size() == 0)
    {
        return;
    }
    if (this->qImage == 0)
    {
        this->qImage = new QImage(frame->width, frame->height, QImage::Format_ARGB32);

    }
    this->frameMutex->lock();
    if (*(this->dataReady))
    {
        this->convertToQImage(frame);
        *(this->dataReady) = false;
    }
    this->frameMutex->unlock();
    if (this->buff->size())
    {
        this->buff->reset();
    }
    cvShowImage("sender_window", frame);
    this->qImage->save(this->buff, "JPG");    // save image to buffer
    this->sendImage();
}

void Sender::convertToQImage(IplImage *frame)
{
    int h = frame->height;
    int w = frame->width;
    //    static QImage qImage(w, h, QImage::Format_ARGB32);
    int channels = frame->nChannels;
    char *data = frame->imageData;

    for (int y = 0; y < h; y++, data += frame->widthStep)
    {
        for (int x = 0; x < w; x++)
        {
            char r, g, b, a = 0;
            if (channels == 1)
            {
                r = data[x * channels];
                g = data[x * channels];
                b = data[x * channels];
            }
            else if (channels == 3 || channels == 4)
            {
                r = data[x * channels + 2];
                g = data[x * channels + 1];
                b = data[x * channels];
            }

            if (channels == 4)
            {
                a = data[x * channels + 3];
                this->qImage->setPixel(x, y, qRgba(r, g, b, a));
            }
            else
            {
                this->qImage->setPixel(x, y, qRgb(r, g, b));
            }
        }
    }
    return;
}
