/****************************************************************************
** Meta object code from reading C++ file 'recv.h'
**
** Created: Wed Dec 7 16:47:32 2011
**      by: The Qt Meta Object Compiler version 62 (Qt 4.6.3)
**
** WARNING! All changes made in this file will be lost!
*****************************************************************************/

#include "recv.h"
#if !defined(Q_MOC_OUTPUT_REVISION)
#error "The header file 'recv.h' doesn't include <QObject>."
#elif Q_MOC_OUTPUT_REVISION != 62
#error "This file was generated using the moc from 4.6.3. It"
#error "cannot be used with the include files from this version of Qt."
#error "(The moc has changed too much.)"
#endif

QT_BEGIN_MOC_NAMESPACE
static const uint qt_meta_data_Recv[] = {

 // content:
       4,       // revision
       0,       // classname
       0,    0, // classinfo
       2,   14, // methods
       0,    0, // properties
       0,    0, // enums/sets
       0,    0, // constructors
       0,       // flags
       0,       // signalCount

 // slots: signature, parameters, type, tag, flags
       6,    5,    5,    5, 0x09,
      18,    5,    5,    5, 0x09,

       0        // eod
};

static const char qt_meta_stringdata_Recv[] = {
    "Recv\0\0connected()\0readImage()\0"
};

const QMetaObject Recv::staticMetaObject = {
    { &QThread::staticMetaObject, qt_meta_stringdata_Recv,
      qt_meta_data_Recv, 0 }
};

#ifdef Q_NO_DATA_RELOCATION
const QMetaObject &Recv::getStaticMetaObject() { return staticMetaObject; }
#endif //Q_NO_DATA_RELOCATION

const QMetaObject *Recv::metaObject() const
{
    return QObject::d_ptr->metaObject ? QObject::d_ptr->metaObject : &staticMetaObject;
}

void *Recv::qt_metacast(const char *_clname)
{
    if (!_clname) return 0;
    if (!strcmp(_clname, qt_meta_stringdata_Recv))
        return static_cast<void*>(const_cast< Recv*>(this));
    return QThread::qt_metacast(_clname);
}

int Recv::qt_metacall(QMetaObject::Call _c, int _id, void **_a)
{
    _id = QThread::qt_metacall(_c, _id, _a);
    if (_id < 0)
        return _id;
    if (_c == QMetaObject::InvokeMetaMethod) {
        switch (_id) {
        case 0: connected(); break;
        case 1: readImage(); break;
        default: ;
        }
        _id -= 2;
    }
    return _id;
}
QT_END_MOC_NAMESPACE
