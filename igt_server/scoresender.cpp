#include "scoresender.h"

ScoreSender::ScoreSender(QString scoreFilePath, QObject *parent) :
        QThread(parent)
        ,port(2345)
        ,numCard(0)
{
    this->scoreConnections = new QList<QTcpSocket*>();
    this->scoreServer = new QTcpServer(this);

}

void ScoreSender::run()
{
    this->scoreServer->listen(QHostAddress::Any, this->port);
    connect(this->scoreServer, SIGNAL(newConnection()),  this, SLOT(handleNewConnection()));
    qDebug() << "Start Listening on port:" << this->port;


    exec();
}

void ScoreSender::handleNewConnection()
{
    QTcpSocket *t = this->scoreServer->nextPendingConnection();
    this->scoreConnections->append(t);
    connect(t, SIGNAL(readyRead()), this, SLOT(sendScore()));
}

void ScoreSender::sendScore()
{
    QObject *emitter = QObject::sender();
    QTcpSocket *t = 0;
    char selected;
    if (emitter->inherits("QTcpSocket"))
    {
        t = static_cast<QTcpSocket *>(emitter);
    }
    QByteArray selectedDeck = t->readAll();
    if (selectedDeck.size())
    {
        selected = selectedDeck.at(0);
    }
    qDebug() << "Card: " << this->numCard << " Deck: " << selected;
    this->numCard = this->numCard + 1;
    // TODO send result to
    switch(selected)
    {
    case 'A':
    case 'a':
        break;
    case 'B':
    case 'b':
        break;
    case 'C':
    case 'c':
        break;
    case 'D':
    case 'd':
        break;
    }
}
