#include "igtsubject.h"
#include <QFile>
#include <QTextStream>
#include <QDebug>

IGTSubject::IGTSubject()
{

}

void IGTSubject::setAge(int age)
{
    this->age = age;
}

void IGTSubject::setEMail(QString email)
{
    this->email = email;
}

void IGTSubject::setFamily(QString family)
{
    this->family = family;
}

void IGTSubject::setGender(QString gender)
{
    this->gender = gender;
}

int IGTSubject::getId()
{
    return this->id;
}

void IGTSubject::setID(int id)
{
    this->id = id;
}

void IGTSubject::setMobile(QString mobile)
{
    this->mobile = mobile;
}

void IGTSubject::setName(QString name)
{
    this->name = name;
}

void IGTSubject::setRorL(QString RorL)
{
    this->RorL = RorL;
}

void IGTSubject::setStdID(int stdID)
{
    this->stdID = stdID;
}

void IGTSubject::addAction(char action, float time)
{
    this->actions.append(action);
    this->actionTimes.append(time);
}

int IGTSubject::save(QString path)
{
    bool stat;
    QFile file(path.append(QString::number(this->id)));
    qDebug() << "saving file in: " << path;
    stat = file.open(QIODevice::WriteOnly | QIODevice::Text);
    if (!stat)
    {
        qDebug() << "Couldn't open file for subject data";
        return 1;
    }

    QTextStream out(&file);
    out << "# subject id: " << this->id << "\n";
    out << "# subject name: " << this->name << "\n";
    out << "# subject family: " << this->family << "\n";
    out << "# subject stdID: " << this->stdID << "\n";
    out << "# subject EMail: " << this->email << "\n";
    out << "# subject Mobile: " << this->mobile << "\n";
    out << "# subject Gender: " << this->gender << "\n";
    out << "# subject Age: " << this->age << "\n";
    out << "# subject RorL: " << this->RorL << "\n";

    out << "# Action Times:\n";
    int i;
    for(i = 0; i < this->actions.size(); i++)
    {
        out << this->actions.at(i) << " " << this->actionTimes.at(i) << "\n";
    }

    file.close();
    return 0;
}


