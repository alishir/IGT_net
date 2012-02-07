#ifndef SENDER_H
#define SENDER_H

#include <QThread>
#include <QImage>
#include <QTcpSocket>
#include <QTcpServer>
#include <QByteArray>
#include <QMutex>
#include <QBuffer>
#include <QFileSystemWatcher>

#include "cv.h"
//#include "highgui.h"


class Sender : public QThread
{
    Q_OBJECT
public:
    Sender(QMutex *frameMutex, bool *dataReady, QObject *parent);

protected:
    void run();
    void convertToQImage(IplImage *frame);

public slots:
    void getFrame(IplImage *frame);

protected slots:
    void handleNewConnection();
    void sendImageFile(QString fn);
    void clientDisconnected();
    void sendComplete(qint64 bytes);



private:
    void sendImage();
    bool *dataReady;
    QTcpServer *camServer;
    QList<QTcpSocket*> *camConnections;
    QImage* qImage;
    QFileSystemWatcher *imageWatcher;
    QByteArray *ba;
    QBuffer *buff;
    QMutex *frameMutex;
    bool sendNext;
};

#endif // SENDER_H
