// -*- c-basic-offset: 8; indent-tabs: nil -*-
//
// See LICENSE for details.
//
// TBD: as workaround for SBCL: don't invoke callbacks when not in an SBCL thread
#include <smoke.h>
#include <smoke/qtcore_smoke.h>
#include <QtCore>
#include <QtGui>
#include "commonqt.h"

// #define DEBUG 1

#include <iostream>
#include <string>
#include <stdlib.h>

using namespace std;

typedef void (*t_deletion_callback)(void*, void*);
typedef bool (*t_callmethod_callback)(void*, short, void*, void*, bool);
typedef void (*t_child_callback)(void*, bool, void*);

void dummy_deletion_callback(void*, void*) {}
bool dummy_callmethod_callback(void*, short, void*, void*, bool) { return false; }
void dummy_child_callback(void*, bool, void*) {}

class Binding : public SmokeBinding
{
public:
        Binding(Smoke* s) : SmokeBinding(s) {}

        t_deletion_callback deletion_callback;
        t_callmethod_callback callmethod_callback;
	t_child_callback child_callback;

        void deleted(Smoke::Index, void* obj) {
                deletion_callback(smoke, obj);
        }

        bool callMethod(Smoke::Index method, void* obj,
                Smoke::Stack args, bool isAbstract)
        {
		Smoke::Method* m = &smoke->methods[method];
		const char* name = smoke->methodNames[m->name];
		Smoke::Class* c = &smoke->classes[m->classId];

		if (*name == '~')
			callmethod_callback(smoke, method, obj, args, isAbstract);
		else if (!strcmp(name, "notify")
			 && (!strcmp(c->className, "QApplication")
			    || !strcmp(c->className, "QCoreApplication")))
		{
			QEvent* e = (QEvent*) args[2].s_voidp;
			if (e->type() == QEvent::ChildAdded
			    || e->type() == QEvent::ChildRemoved)
			{
				QChildEvent* f = (QChildEvent*) e;
				child_callback(smoke, f->added(), f->child());
			}
		}
		return false;
	}

        char* className(Smoke::Index classId) {
                return (char*) smoke->classes[classId].className;
        }
};

class DynamicBinding : public Binding
{
public:
        DynamicBinding(Smoke* s) : Binding(s) {}

        bool callMethod(Smoke::Index method, void* obj,
                Smoke::Stack args, bool isAbstract)
        {
                return callmethod_callback(smoke, method, obj, args, isAbstract);
        }
};

void
sw_smoke(Smoke* smoke,
	 SmokeData* data,
	 void* deletion_callback,
	 void* method_callback,
	 void* child_callback)
{
        Binding* binding = new Binding(smoke);
        DynamicBinding* dynamicBinding = new DynamicBinding(smoke);

	data->name = smoke->moduleName();

        data->classes = smoke->classes;
        data->numClasses = smoke->numClasses;

        data->methods = smoke->methods;
        data->numMethods = smoke->numMethods;

        data->methodMaps = smoke->methodMaps;
        data->numMethodMaps = smoke->numMethodMaps;

        data->methodNames = smoke->methodNames;
        data->numMethodNames = smoke->numMethodNames;

        data->types = smoke->types;
        data->numTypes = smoke->numTypes;

        data->inheritanceList = smoke->inheritanceList;
        data->argumentList = smoke->argumentList;
        data->ambiguousMethodList = smoke->ambiguousMethodList;
        data->castFn = (void *) smoke->castFn;

	dynamicBinding->deletion_callback
		= (t_deletion_callback) deletion_callback;

        dynamicBinding->callmethod_callback
                = (t_callmethod_callback) method_callback;

	dynamicBinding->child_callback
		= (t_child_callback) child_callback;

	binding->deletion_callback = dynamicBinding->deletion_callback;
	binding->callmethod_callback = dynamicBinding->callmethod_callback;
	binding->child_callback = dynamicBinding->child_callback;

        data->thin = binding;
        data->fat = dynamicBinding;
}

void sw_shutdown (SmokeData* data)
{
        static_cast<Binding*>(data->fat)->deletion_callback = dummy_deletion_callback;
        static_cast<Binding*>(data->fat)->callmethod_callback = dummy_callmethod_callback;
        static_cast<Binding*>(data->fat)->child_callback = dummy_child_callback;
        static_cast<Binding*>(data->thin)->deletion_callback = dummy_deletion_callback;
        static_cast<Binding*>(data->thin)->callmethod_callback = dummy_callmethod_callback;
        static_cast<Binding*>(data->thin)->child_callback = dummy_child_callback;
}

int
sw_windows_version()
{
#ifdef WINDOWS
	return QSysInfo::windowsVersion();
#else
	return -1;
#endif
}

void sw_install_message_handler(QtMsgHandler handler)
{
     qInstallMsgHandler(handler);
}

// void*
// sw_make_qstring(char *str)
// {
//         QString* qstr = new QString();
//         *qstr = QString::fromUtf8(str);
//         return qstr;
// }

void*
sw_make_qbytearray(char* str, void* place)
{
        if (place) {
                *reinterpret_cast<QByteArray*>(place) = QByteArray(str);
                return place;
        }
        return new QByteArray(str);
}

void
sw_delete_qbytearray(void *q)
{
        delete (QByteArray*) q;
}

void*
sw_make_qstring(char *str, void* place)
{
        if (place) {
                *reinterpret_cast<QString*>(place) = QString::fromUtf8(str);
                return place;
        }
        return new QString(QString::fromUtf8(str));
}

void*
sw_qstring_to_utf8(void* s)
{
        QString* str = (QString*) s;
        return new QByteArray(str->toUtf8());
}

const void*
sw_qstring_to_utf16(void* s)
{
        QString* str = static_cast<QString*>(s);
	return str->utf16();
}

void
sw_delete_qstring(void *q)
{
        delete (QString*) q;
}

void*
sw_make_metaobject(void *p, char *strings, int *d)
{
        QMetaObject* parent = (QMetaObject*) p;
        const uint* data = (const uint*) d;

        QMetaObject tmp = { { parent, strings, data, 0 } };
        QMetaObject* ptr = new QMetaObject;
        *ptr = tmp;
        return ptr;
}

void
sw_delete(void *p)
{
        QObject* q = (QObject*) p;
        delete q;
}

typedef void (*t_ptr_callback)(void *);

void
sw_find_class(char *name, Smoke **smoke, short *index)
{
	Smoke::ModuleIndex mi = qtcore_Smoke->findClass(name);
	*smoke = mi.smoke;
	*index = mi.index;
}

void
sw_id_instance_class(void *ptr, Smoke **smoke, short *index)
{
	Smoke::ModuleIndex mi = qtcore_Smoke->findClass(((QObject*)ptr)->metaObject()->className());
	*smoke = mi.smoke;
	*index = mi.index;
}

short
sw_find_name(Smoke *smoke, char *name)
{
	Smoke::ModuleIndex mi = smoke->idMethodName(name);
	return mi.index;
}

short
sw_id_method(Smoke *smoke, short classIndex, short name)
{
	Smoke::ModuleIndex mi = smoke->idMethod(classIndex, name);
	return mi.index;
}

short
sw_id_type(Smoke *smoke, char *name)
{
	return smoke->idType(name);
}

short
sw_id_class(Smoke *smoke, char *name, bool external)
{
	return smoke->idClass(name, external).index;
}

// QStringList marshalling

void*
sw_qstringlist_new(void *place)
{
        if (place) {
                *reinterpret_cast<QStringList*>(place) = QStringList();
                return place;
        }
        return new QStringList();
}

int
sw_qstringlist_size(void* q)
{
	return static_cast<QStringList*>(q)->size();
}

void
sw_qstringlist_delete(void *q)
{
	delete static_cast<QStringList*>(q);
}

const void*
sw_qstringlist_at(void* q, int index)
{
	return static_cast<QStringList*>(q)->at(index).utf16();
}

void
sw_qstringlist_append(void *q, char *x)
{
	static_cast<QStringList*>(q)->append(QString(QString::fromUtf8(x)));
}
        
// QList<Something*> marshalling

void*
sw_qlist_void_new(void *place)
{
        if (place) {
                *reinterpret_cast<QList<void*>*>(place) = QList<void*>();
                return place;
        }
	return new QList<void*>;
}

int
sw_qlist_void_size(void *ptr)
{
	QList<void*>* qlist = static_cast<QList<void*>*>(ptr);
	return qlist->size();
}

void
sw_qlist_void_delete(void *ptr)
{
	QList<void*>* qlist = static_cast<QList<void*>*>(ptr);
	delete qlist;
}

const void*
sw_qlist_void_at(void *ptr, int index)
{
	QList<void*>* qlist = static_cast<QList<void*>*>(ptr);
	return qlist->at(index);
}

void
sw_qlist_void_append(void *ptr, void *whatptr)
{
	QList<void*>* qlist = static_cast<QList<void*>*>(ptr);
	qlist->append(whatptr);
}

// QList<scalar> marshalling

#define DEFINE_QLIST_SCALAR_MARSHALLER(ELEMENT_TYPE, NAME) \
  void* \
  sw_qlist_##NAME##_new(void * place) \
  { \
          if (place) { \
                  *reinterpret_cast<QList<ELEMENT_TYPE>*>(place) = QList<ELEMENT_TYPE>(); \
                  return place; \
          } \
          return new QList<ELEMENT_TYPE>; \
  } \
  int \
  sw_qlist_##NAME##_size(void *ptr) \
  { \
        QList<ELEMENT_TYPE>* qlist = static_cast<QList<ELEMENT_TYPE>*>(ptr); \
  	return qlist->size(); \
  } \
  void \
  sw_qlist_##NAME##_delete(void *ptr) \
  { \
  	QList<ELEMENT_TYPE>* qlist = static_cast<QList<ELEMENT_TYPE>*>(ptr); \
  	delete qlist; \
  } \
  const void* \
  sw_qlist_##NAME##_at(void *ptr, int index) \
  { \
  	QList<ELEMENT_TYPE>* qlist = static_cast<QList<ELEMENT_TYPE>*>(ptr); \
  	return &qlist->at(index); \
  } \
  void \
  sw_qlist_##NAME##_append(void *ptr, void *whatptr) \
  { \
  	QList<ELEMENT_TYPE>* qlist = static_cast<QList<ELEMENT_TYPE>*>(ptr); \
  	qlist->append(*static_cast<ELEMENT_TYPE*>(whatptr)); \
  }

DEFINE_QLIST_SCALAR_MARSHALLER(int, int)
DEFINE_QLIST_SCALAR_MARSHALLER(QVariant, qvariant)
DEFINE_QLIST_SCALAR_MARSHALLER(QByteArray, qbytearray)
DEFINE_QLIST_SCALAR_MARSHALLER(QModelIndex, qmodelindex)
DEFINE_QLIST_SCALAR_MARSHALLER(QKeySequence, qkeysequence)

// QMap<scalar, scalar> marshalling

#define DEFINE_QMAP_SCALAR_MARSHALLER(KEY_TYPE, VALUE_TYPE, NAME)      \
  void* \
  sw_qmap_##NAME##_new(void * place) \
  { \
          if (place) { \
                  *reinterpret_cast<QMap<KEY_TYPE, VALUE_TYPE>*>(place) = QMap<KEY_TYPE, VALUE_TYPE>(); \
                  return place; \
          } \
          return new QMap<KEY_TYPE, VALUE_TYPE>;      \
  } \
  void \
  sw_qmap_##NAME##_delete(void *ptr) \
  { \
  	QMap<KEY_TYPE, VALUE_TYPE>* qmap = static_cast<QMap<KEY_TYPE, VALUE_TYPE>*>(ptr); \
  	delete qmap; \
  } \
  void \
  sw_qmap_##NAME##_map(void *ptr, map_func f) \
  { \
  	QMap<KEY_TYPE, VALUE_TYPE>* qmap = static_cast<QMap<KEY_TYPE, VALUE_TYPE>*>(ptr); \
        QMapIterator<KEY_TYPE, VALUE_TYPE> iter(*qmap); \
        while (iter.hasNext()) { \
                iter.next(); \
                f(&iter.key(), &iter.value()); \
        } \
  } \
  void \
  sw_qmap_##NAME##_set(void *ptr, void *key, void *value)   \
  { \
  	QMap<KEY_TYPE, VALUE_TYPE>* qmap = static_cast<QMap<KEY_TYPE, VALUE_TYPE>*>(ptr); \
  	(*qmap)[*static_cast<KEY_TYPE*>(key)] = *static_cast<VALUE_TYPE*>(value); \
  }

DEFINE_QMAP_SCALAR_MARSHALLER(QString, QVariant, qstring_qvariant);
