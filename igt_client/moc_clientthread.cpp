/****************************************************************************
** Meta object code from reading C++ file 'clientthread.h'
**
** Created: Sun Feb 5 19:36:00 2012
**      by: The Qt Meta Object Compiler version 62 (Qt 4.6.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "clientthread.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'clientthread.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_ClientThread[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       4,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       1,       // signalCount

 // signals: signature, parameters, type, tag, flags
      20,   14,   13,   13, 0x05,

 // slots: signature, parameters, type, tag, flags
      41,   13,   13,   13, 0x08,
      52,   13,   13,   13, 0x08,
      64,   13,   13,   13, 0x08,

       0        // eod
};

static const char qt_meta_stringdata_ClientThread[] = {
    "ClientThread\0\0image\0updateImage(QPixmap)\0"
    "getImage()\0connected()\0disconnected()\0"
};

const QMetaObject ClientThread::staticMetaObject = {
    { &QThread::staticMetaObject, qt_meta_stringdata_ClientThread,
      qt_meta_data_ClientThread, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &ClientThread::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *ClientThread::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *ClientThread::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_ClientThread))
        return static_cast<void*>(const_cast< ClientThread*>(this));
    return QThread::qt_metacast(_clname);
}

int ClientThread::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QThread::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: updateImage((*reinterpret_cast< QPixmap(*)>(_a[1]))); break;
        case 1: getImage(); break;
        case 2: connected(); break;
        case 3: disconnected(); break;
        default: ;
        }
        _id -= 4;
    }
    return _id;
}

// SIGNAL 0
void ClientThread::updateImage(QPixmap _t1)
{
    void *_a[] = { 0, const_cast<void*>(reinterpret_cast<const void*>(&_t1)) };
    QMetaObject::activate(this, &staticMetaObject, 0, _a);
}
QT_END_MOC_NAMESPACE
