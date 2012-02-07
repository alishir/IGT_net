#ifndef IGTSUBJECT_H
#define IGTSUBJECT_H

#include <QString>
#include <QList>

class IGTSubject
{
public:
    IGTSubject();
    void setID(int id);
    void setStdID(int stdID);
    void setAge(int age);
    void setGender(QString gender);
    void setRorL(QString RorL);
    void setName(QString name);
    void setFamily(QString family);
    void setEMail(QString email);
    void setMobile(QString mobile);


    int getId();

    void addAction(char action, float time);
    int save(QString path);

private:
    int id;
    int stdID;
    int age;
    QString gender;
    QString RorL;
    QString name;
    QString family;
    QString email;
    QString mobile;

    QList<char> actions;
    QList<float> actionTimes;
};

#endif // IGTSUBJECT_H
