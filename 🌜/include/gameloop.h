#ifndef GAMELOOP_H
#define GAMELOOP_H

/** integral type that counts frames. */
typedef unsigned long frame_t;
/** Callback that is run each frame. */
typedef void (*frame_function_t)(frame_t);

/** Starts the gameloop */
frame_t gameloop_start(frame_function_t func);
/** Stops the gameloop. */
frame_t gameloop_stop();

#endif /* GAMELOOP_H */
