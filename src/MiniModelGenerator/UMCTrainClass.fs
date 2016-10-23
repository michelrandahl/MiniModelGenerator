module UMCTrain

let class_definition = """
Class Train is

  Signals:
    ok, no;

  Vars:
    requested_point_positions : bool[];
    train_length : int = 2; -- how many track segments does the train occupy
    route_segments : obj[];
    route_index : int := 0; -- current location on the route
    occupies : obj[]; -- the tc and pt objects which the train currently occupies
    front_advancement_count : int; -- a variable for keeping track of the trains front advancement over a track
    track_lengths : int[]; -- same number of elements as route_segments

  State Top = READY, WAIT_OK, MOVEMENT, ARRIVED

  Transitions:
    -- send out initial reservation request to the first node on route
    READY -> WAIT_OK {
		- /
		route_segments[0].req(self, 0, route_segments, requested_point_positions);
	}

	-- when the train reservation is rejected we just keep cycling between WAIT_OK and READY
    WAIT_OK -> READY { no }

    -- train receives ackknowledgement that the route has been reserved succesfully
    -- the front_advancement_count variable is initialized to reflect the trains front location on the track
    WAIT_OK -> MOVEMENT { ok / front_advancement_count := train_length - 1; }

    MOVEMENT -> MOVEMENT {
		-
		[not (route_index = route_segments.length - 1 and
		 track_lengths[route_index] - 1 = front_advancement_count)] / -- at end of track
        at_end_of_track: bool := track_lengths[route_index] - 1 = front_advancement_count; -- determine if we have reached the end of the current track
		if at_end_of_track = true then { -- the train has reached the end of its current track
          front_advancement_count := 0;
		  if route_index < route_segments.length - 1 then { -- the route_index is not the last
            -- train enters next track
            route_index := route_index + 1;
		    route_segments[route_index].sensorOn(self); -- the next track detects the train
		  };
		} else {
          front_advancement_count := front_advancement_count + 1;
		};
        -- update the occupies array
		rear: obj := occupies.head;
        next_rear: obj := occupies.tail.head;
        occupies := occupies.tail + [route_segments[route_index]];
	    if rear != next_rear then { -- determine if the rear of the train has left a track
          rear.sensorOff(self); -- the past track detects that the train does not occupy it anymore
	    };
	}

    MOVEMENT -> ARRIVED {
		-
		[route_index = route_segments.length - 1 and -- at last track segment of route
		 track_lengths[route_index] - 1 = front_advancement_count] -- at end of track
	}

end Train
"""
