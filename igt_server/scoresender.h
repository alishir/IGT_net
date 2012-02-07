#ifndef SCORESENDER_H
#define SCORESENDER_H

#include <QThread>
#include <QTcpServer>
#include <QTcpSocket>
#include <QList>

class ScoreSender : public QThread
{
Q_OBJECT
public:
    explicit ScoreSender(QString scoreFilePath, QObject *parent = 0);

signals:

public slots:

protected slots:
    void handleNewConnection();
    void sendScore();

protected:
    void run();

private:
    QTcpServer *scoreServer;
    QList<QTcpSocket *> *scoreConnections;
    int port;
    int numCard;

};

#endif // SCORESENDER_H
