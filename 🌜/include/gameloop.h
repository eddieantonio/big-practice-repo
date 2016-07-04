/*
 * Copyright (C) 2016 Eddie Antonio Santos <easantos@ualberta.ca>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

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
