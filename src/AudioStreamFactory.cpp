/*
 Copyright © Grame 2003

 This library is free software; you can redistribute it and modify it under
 the terms of the GNU Library General Public License as published by the
 Free Software Foundation version 2 of the License, or any later version.

 This library is distributed in the hope that it will be useful, but
 WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public License
 for more details.

 You should have received a copy of the GNU Library General Public License
 along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

 Grame Research Laboratory, 9, rue du Garet 69001 Lyon - France
 grame@rd.grame.fr

*/

#include "AudioStreamFactory.h"
#include "TAudioStreamFactory.h"
#include "TJavaAudioStream.h"

/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    Init
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_grame_sound_audioengine_AudioStreamFactory_Init
(JNIEnv *, jclass)
{
    TAudioStreamFactory::Init();
}

/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    Destroy
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_grame_sound_audioengine_AudioStreamFactory_Destroy
(JNIEnv *, jclass)
{
    TAudioStreamFactory::Destroy();
}

/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    MakeNullSound
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_grame_sound_audioengine_AudioStreamFactory_MakeNullSound
(JNIEnv *, jclass, jint len)
{
    return (jint)TAudioStreamFactory::MakeNullSound(len);
}


/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    MakeReadSound
 * Signature: (Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_grame_sound_audioengine_AudioStreamFactory_MakeReadSound
(JNIEnv * env, jclass, jstring jname)
{
    const char * name = env->GetStringUTFChars(jname, 0);
    TAudioStreamPtr s = TAudioStreamFactory::MakeReadSound((char *)name);
    env->ReleaseStringUTFChars(jname, name);
    return (jint)s;
}


/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    MakeWriteSound
 * Signature: (Ljava/lang/String;I)I
 */
JNIEXPORT jint JNICALL Java_grame_sound_audioengine_AudioStreamFactory_MakeWriteSound
(JNIEnv * env, jclass, jstring jname, jint s1)
{
    const char * name = env->GetStringUTFChars(jname, 0);
    TAudioStreamPtr s2 = TAudioStreamFactory::MakeWriteSound((char *)name, (TAudioStreamPtr)s1);
    env->ReleaseStringUTFChars(jname, name);
    return (jint)s2;
}


/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    MakeRegionSound
 * Signature: (Ljava/lang/String;II)I
 */
JNIEXPORT jint JNICALL Java_grame_sound_audioengine_AudioStreamFactory_MakeRegionSound
(JNIEnv * env, jclass, jstring jname, jint begin, jint end)
{
    const char * name = env->GetStringUTFChars(jname, 0);
    TAudioStreamPtr s = TAudioStreamFactory::MakeRegionSound((char *)name, begin, end);
    env->ReleaseStringUTFChars(jname, name);
    return (jint)s;
}


/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    MakeLoopSound
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_grame_sound_audioengine_AudioStreamFactory_MakeLoopSound
(JNIEnv *, jclass, jint s, jint n)
{
    return (jint)TAudioStreamFactory::MakeLoopSound((TAudioStreamPtr)s, n);
}


/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    MakeFadeSound
 * Signature: (III)I
 */
JNIEXPORT jint JNICALL Java_grame_sound_audioengine_AudioStreamFactory_MakeFadeSound
(JNIEnv *, jclass, jint s, jint in, jint out)
{
    return (jint)TAudioStreamFactory::MakeFadeSound((TAudioStreamPtr)s, in, out);
}


/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    MakeCutSound
 * Signature: (III)I
 */
JNIEXPORT jint JNICALL Java_grame_sound_audioengine_AudioStreamFactory_MakeCutSound
(JNIEnv *, jclass, jint s, jint begin, jint end)
{
    return (jint)TAudioStreamFactory::MakeCutSound((TAudioStreamPtr)s, begin, end);
}


/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    MakeSeqSound
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_grame_sound_audioengine_AudioStreamFactory_MakeSeqSound
(JNIEnv *, jclass, jint s1, jint s2)
{
    return (jint)TAudioStreamFactory::MakeSeqSound((TAudioStreamPtr)s1, (TAudioStreamPtr)s2);
}


/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    MakeMixSound
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_grame_sound_audioengine_AudioStreamFactory_MakeMixSound
(JNIEnv *, jclass, jint s1, jint s2)
{
    return (jint)TAudioStreamFactory::MakeMixSound((TAudioStreamPtr)s1, (TAudioStreamPtr)s2);
}


/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    MakeTransformSound
 * Signature: (IIII)I
 */
JNIEXPORT jint JNICALL Java_grame_sound_audioengine_AudioStreamFactory_MakeTransformSound
(JNIEnv *, jclass, jint s, jint e, jint begin, jint end)
{
    return (jint)TAudioStreamFactory::MakeTransformSound((TAudioStreamPtr)s, (TAudioEffectPtr)e, begin, end);
}


/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    MakeConvertSound
 * Signature: (IID)I
 */
JNIEXPORT jint JNICALL Java_grame_sound_audioengine_AudioStreamFactory_MakeConvertSound
(JNIEnv *, jclass, jint s, jint c, jdouble ratio)
{
    return (jint)TAudioStreamFactory::MakeConvertSound((TAudioStreamPtr)s, c, ratio);
}


/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    MakeInputSound
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_grame_sound_audioengine_AudioStreamFactory_MakeInputSound
(JNIEnv *, jclass)
{
    return (jint)TAudioStreamFactory::MakeInputSound();
}


/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    MakeNativeSound
 * Signature: (Lgrame/sound/audioengine/TAudioStream;)I
 */
JNIEXPORT jint JNICALL Java_grame_sound_audioengine_AudioStreamFactory_MakeNativeSound
(JNIEnv * env, jclass, jobject obj)
{
    return (jint) new TJavaAudioStream(env, obj);
}


/*
 * Class:     grame_sound_audioengine_AudioStreamFactory
 * Method:    DeleteSound
 * Signature: (I)I
 */
JNIEXPORT void JNICALL Java_grame_sound_audioengine_AudioStreamFactory_DeleteSound
(JNIEnv *, jclass, jint obj)
{
    delete((TAudioStreamPtr)obj);
}


