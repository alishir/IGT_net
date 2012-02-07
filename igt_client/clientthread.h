#ifndef CLIENTTHREAD_H
#define CLIENTTHREAD_H

#include <QThread>
#include <QTimer>
#include <QTcpSocket>
#include <QHostAddress>
#include <QPixmap>
#include <QByteArray>
#include <QBuffer>
#include <cxcore.h>

class ClientThread : public QThread
{
    Q_OBJECT
public:
    ClientThread(QString serverIP, int port, QObject* parent);
    ~ClientThread();

signals:
    void updateImage(QPixmap image);

private slots:
    void getImage();
    void connected();
    void disconnected();

protected:
    void run();

private:
    QObject *parent;
    QTcpSocket *tcpConnection;
    QString serverAddress;
    int serverPort;
    QByteArray *ba;
    QByteArray *imageData;      // size of remain data to construct image
    int imageDataSize;
    QBuffer *buff;

};

#endif // CLIENTTHREAD_H
