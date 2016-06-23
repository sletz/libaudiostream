/*

Copyright (C) Grame 2002-2014

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
research@grame.fr

*/

#ifndef __TExpAudioMixer__
#define __TExpAudioMixer__

#include "TAudioClient.h"
#include "TRendererAudioStream.h"
#include "TAudioGlobals.h"
#include "TAudioDate.h"
#include "la_smartpointer.h"

#include <list>
#include <atomic>
#include <map>

//----------------
// Class TCommand
//----------------

struct TCommand : public la_smartable1 {

        SymbolicDate fStartDate;

		inline audio_frame_t GetDate(std::map<SymbolicDate, audio_frame_t>& date_map, SymbolicDate date)
        {
            if (date_map.find(date) == date_map.end()) {
                date_map[date] = date->getDate();
            }
            return date_map[date];
        }

        inline bool InBuffer(audio_frame_t date, audio_frame_t cur_frame, long frames)
        {
            return (date >= cur_frame && date < cur_frame + frames);
        }

        inline audio_frame_t GetDate() { return fStartDate->GetDate(); }

        TCommand()
        {}
        TCommand(SymbolicDate date):fStartDate(date)
        {}
        virtual ~TCommand()
        {}

        virtual bool Execute(TNonInterleavedAudioBuffer<float>* buffer,
		                    std::map<SymbolicDate, audio_frame_t>& date_map,
                            audio_frame_t cur_frame,
                            long frames) = 0;

        bool operator< (const TCommand& command) const
        {
            return fStartDate->getDate() < command.fStartDate->getDate();
        }

        virtual long GetOffset(audio_frame_t cur_frame, long frames) { return -1; }

};

typedef LA_SMARTP<TCommand> TCommandPtr;


//--------------------------------------------
// Class TControlCommand : a control command
//--------------------------------------------

struct TControlCommand : public TCommand {

        TControlCommand()
        {}
        TControlCommand(SymbolicDate date):TCommand(date)
        {}
        virtual ~TControlCommand()
        {}

        /*
            Returns the offset in buffer, or frames if not in buffer.
        */
        virtual long GetOffset(audio_frame_t cur_frame, long frames)
        {
            if (InBuffer(fStartDate->getDate(), cur_frame, frames)) {
                return fStartDate->getDate() - cur_frame;
            } else {
                return frames;
            }
        }

};

typedef LA_SMARTP<TControlCommand> TControlCommandPtr;

//---------------------------------------------------------------------------
// Class TEffectControlCommand : a command to set Faust effect control value
//---------------------------------------------------------------------------

struct TEffectControlCommand : public TControlCommand {

        TAudioEffectInterfacePtr fEffect;
		std::string fPath;
        float fValue;

        TEffectControlCommand()
        {}
		TEffectControlCommand(TAudioEffectInterfacePtr effect, const std::string& path, float value, SymbolicDate date)
            : TControlCommand(date), fEffect(effect), fPath(path), fValue(value)
        {}
        virtual ~TEffectControlCommand()
        {}

        bool Execute(TNonInterleavedAudioBuffer<float>* buffer,
		            std::map<SymbolicDate, audio_frame_t>& date_map,
                    audio_frame_t cur_frame,
                    long frames)
        {
            if (InBuffer(fStartDate->getDate(), cur_frame, frames)) {
                //printf("SetControlValue %s %f\n", fPath.c_str(), fValue);
                fEffect->SetControlValue(fPath.c_str(), fValue);
                return false;
            } else {
                return true;
            }
        }

};

//-------------------------------------------------------------------------
// Class TExternalControlCommand : a command to call an user given callback
//-------------------------------------------------------------------------

typedef void (*AudioControlCallback) (audio_frame_t date, float value, void *arg);

struct TExternalControlCommand : public TCommand {

        AudioControlCallback fCallback;
        void* fArg;
        float fValue;

        TExternalControlCommand(AudioControlCallback callback, float value, void* arg)
            :fCallback(callback), fArg(arg), fValue(value)
        {}
        virtual ~TExternalControlCommand()
        {}

        bool Execute(TSharedNonInterleavedAudioBuffer<float>& shared_buffer,
		            std::map<SymbolicDate, audio_frame_t>& date_map,
                    audio_frame_t cur_frame,
                    long frames)
        {
            if (InBuffer(fStartDate->getDate(), cur_frame, frames)) {
                fCallback(fStartDate->getDate(), fValue, fArg);
                return false;
            } else {
                return true;
            }
        }

};

//---------------------------------------------------------
// Class TStreamCommand : a command to start/stop streams
//---------------------------------------------------------

struct TStreamCommand : public TCommand {

        TRTRendererAudioStreamPtr fStream; // SmartPtr here...

        long fPos;

        SymbolicDate fStopDate;

        TStreamCommand(TRTRendererAudioStreamPtr stream, SymbolicDate start_date, SymbolicDate stop_date)
                :TCommand(start_date), fStream(stream), fPos(0), fStopDate(stop_date)
        {}
        virtual ~TStreamCommand()
        {}

        void SetSartDate(SymbolicDate start_date) { fStartDate = start_date; }
        void SetStopDate(SymbolicDate stop_date) { fStopDate = stop_date; }

        bool Execute(TNonInterleavedAudioBuffer<float>* buffer,
		            std::map<SymbolicDate, audio_frame_t>& date_map,
                    audio_frame_t cur_frame,
                    long frames)
        {
            // Keeps the same value for the entire audio cycle
            audio_frame_t start_date = GetDate(date_map, fStartDate);
            audio_frame_t stop_date = GetDate(date_map, fStopDate);

            //printf("%p start_date = %lld stop_date = %lld cur_frame = %lld frames = %ld\n", this, start_date, stop_date, cur_frame, frames);

            // Possibly entire buffer to play
            long start_offset;
            long stop_offset;

            // Init values
            long frame_num;
            bool to_play = false;
            bool to_stop = false;
            long res = 0;

            // Possibly move start_offset inside this buffer
            if (InBuffer(start_date, cur_frame, frames)) {
                // New stream to play...
                to_play = true;
                start_offset = start_date - cur_frame;
                //printf("Start stream fCurFrame = %lld start_offset = %ld\n", cur_frame, start_offset);
            } else if (cur_frame > start_date) {
                // Stream currently playing...
                to_play = true;
                start_offset = 0;
                // If pos is still 0, then we have a too late command...
                if (fPos == 0) {
                    TAudioGlobals::fSchedulingError++;
                }
            } else {
                start_offset = 0;
            }

            // Possibly move stop_offset inside this buffer
            if (InBuffer(stop_date, cur_frame, frames)) {
                // Stream will be stopped in this buffer...
                stop_offset = stop_date - cur_frame;
                to_stop = true;
                //printf("Stop stream fCurFrame = %lld stop_offset = %ld\n", cur_frame, stop_offset);
            } else if (stop_date < cur_frame) {
                // Stop playing...
                stop_offset = 0;
                to_stop = true;
            } else {
                stop_offset = frames;
            }

            // Then compute effective frame number to play
            frame_num = stop_offset - start_offset;

            //printf("TStreamCommand::Execute frame_num = %d start_offset = %d\n", frame_num, start_offset);

            // Play it...
            if (to_play) {
                res = fStream->Read(buffer, frame_num, start_offset);
                fPos += res;
                if (to_stop || (res < frame_num)) {
                    return false;
                }
            }

            return true;
        }

};

typedef LA_SMARTP<TStreamCommand> TStreamCommandPtr;

//--------------------
// Class TCommandList
//--------------------

class TCommandList : public std::list<TCommandPtr>
{

    private:

        static bool compare_command_date (TCommandPtr first, TCommandPtr second)
        {
            return first->GetDate() < second->GetDate();
        }

        std::atomic_bool fNeedSort;

    public:

        TCommandList():fNeedSort(false)
        {}
        virtual ~TCommandList() {}

        void AddCommand(TCommandPtr command)
        {
            push_back(command);
            fNeedSort = true;
        }

        void RemoveCommand(TCommandPtr command)
        {
            remove(command);
            fNeedSort = true;
        }

        void PossiblySort()
        {
            if (fNeedSort) {
                sort(compare_command_date);
                fNeedSort = false;
                //printf("SORT\n");
            }
        }

        void NeedSort()
        {
            fNeedSort = true;
        }

        void Print()
        {
            TCommandList::iterator it;
            for (it = begin(); it != end(); it++) {
                TCommandPtr command = *it;
                printf("Command date %lld\n", command->GetDate());
            }
        }

};

typedef TCommandList COMMANDS;
typedef TCommandList::iterator COMMANDS_ITERATOR;

//----------------------
// Class TExpAudioMixer
//----------------------

class TExpAudioMixer : public TAudioClient
{

    private:
		COMMANDS fStreamCommands;     // std::list of stream commands
		COMMANDS fControlCommands;    // std::list of control commands

        audio_frame_t fCurFrame = 0;

        TAudioEffectInterfacePtr fMasterEffect = nullptr;
        FLOAT_BUFFER fBuffer = nullptr;

        long fOutputChannels = 0;

        bool AudioCallback(float** inputs, float** outputs, long frames);


        void ExecuteControlSlice(TNonInterleavedAudioBuffer<float>* buffer,
		                        std::map<SymbolicDate, audio_frame_t>& date_map,
                                audio_frame_t cur_frame,
                                long offset,
                                long slice);

        void ExecuteStreamsSlice(TNonInterleavedAudioBuffer<float>* buffer,
		                        std::map<SymbolicDate, audio_frame_t>& date_map,
                                audio_frame_t cur_frame,
                                long offset,
                                long slice);

        long GetNextControlOffset(audio_frame_t cur_frame, long frames);

    public:

        TExpAudioMixer():
            TExpAudioMixer{
                TAudioGlobals::fBufferSize,
                TAudioGlobals::fOutput}
        {
        }

        TExpAudioMixer(long buffer_size, long output_channels):
            fBuffer{new TLocalNonInterleavedAudioBuffer<float>{
                            buffer_size,
                            output_channels}},
            fOutputChannels{output_channels}
        {

        }

        virtual ~TExpAudioMixer()
        {
            delete fBuffer;
        }

        virtual void AddStreamCommand(TCommandPtr command)
        {
            fStreamCommands.AddCommand(command);
        }
        virtual void StopStreamCommand(TStreamCommandPtr command, SymbolicDate date)
        {
            command->SetStopDate(date);
        }

        virtual void RemoveStreamCommand(TCommandPtr command)
        {
            fStreamCommands.RemoveCommand(command);
        }

        virtual void AddControlCommand(TCommandPtr command)
        {
            fControlCommands.AddCommand(command);
        }
        virtual void RemoveControlCommand(TCommandPtr command)
        {
            fControlCommands.RemoveCommand(command);
        }

        TStreamCommandPtr GetStreamCommand(TAudioStreamPtr stream);

        int GetCommandSize() { return fStreamCommands.size(); }

        void NeedSort()
        {
            fControlCommands.NeedSort();
            fStreamCommands.NeedSort();
        }

        long SetPos(audio_frame_t frames);

        void SetEffect(TAudioEffectInterfacePtr effect) { fMasterEffect = effect; }

};

typedef TExpAudioMixer * TExpAudioMixerPtr;

#endif


