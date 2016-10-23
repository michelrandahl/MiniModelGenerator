module UMCPoint

let class_definition = """
-- points are always intermediate nodes
-- so we don't need to check if they are first or last in the guards
Class Point is

  Signals:
    req(sender: obj, route_index: int, route_elements: obj[], requested_point_positions: bool[]);
    ack(sender: obj);
    nack(sender: obj);
    commit(sender: obj);
    agree(sender: obj);
    disagree(sender: obj);

  Operations:
    sensorOn(sender: obj);
    sensorOff(sender: obj);

  Vars:
    next: obj;
    prev: obj;
    requested_position: bool;
    current_position: bool := True;
    train: obj := null;

  State Top = NON_RESERVED, WAIT_ACK, WAIT_COMMIT, WAIT_AGREE,
		  	  POSITIONING, RESERVED, TRAIN_IN_TRANSITION, MALFUNCTION

 Transitions:

	-- initial reservation request
    NON_RESERVED -> WAIT_ACK {
		req(sender, route_index, route_elements, requested_point_positions) /
		prev := route_elements[route_index - 1];
		next := route_elements[route_index + 1];
		requested_position := requested_point_positions[route_index];
		next.req(self, route_index + 1, route_elements, requested_point_positions);
	}

	-- receiving and forwarding ack
    WAIT_ACK -> WAIT_COMMIT {
		ack(sender) /
		prev.ack(self);
	}

	-- receiving and forwarding commit
    WAIT_COMMIT -> WAIT_AGREE {
		commit(sender) /
		next.commit(self);
	}

	-- if the point is positioned as required for the given route reservation
	-- receiving and forwarding agree
    WAIT_AGREE -> RESERVED {
		agree(sender)
		[current_position = requested_position]/
		prev.agree(self);
	}

	-- if the point is not positioned as required for the given route
	-- goes into positioning state
    WAIT_AGREE -> POSITIONING {
		agree(sender)
		[current_position /= requested_position] /
		-
	}

	-- successfully performing positioning
    POSITIONING -> RESERVED {
		- /
		current_position := not current_position;
		prev.agree(self);
	}

	-- simulating sudden malfunction of positioning system
	-- and sending disagrees to neighbor nodes
    POSITIONING -> MALFUNCTION {
		- /
		prev.disagree(self);
		next.disagree(self);
	}

	-- train moves onto current node
    RESERVED -> TRAIN_IN_TRANSITION {
		sensorOn(sender) /
		train := sender;
	}

	-- sequential release
	-- reset all train
    TRAIN_IN_TRANSITION -> NON_RESERVED {
		sensorOff(sender) /
		-- [sender = train] /
		train := null;
	}

	-- nack received and forwarded
    WAIT_ACK -> NON_RESERVED {
		nack(sender) /
		prev.nack(self);
	}

	-- disagree received and forwarded
    WAIT_COMMIT -> NON_RESERVED {
		disagree(sender) /
		next.disagree(self);
	}

	-- disagree received and forwarded
    WAIT_AGREE -> NON_RESERVED {
		disagree(sender) /
		prev.disagree(self);
	}

	-- disagree received and forwarded
    POSITIONING -> NON_RESERVED {
		disagree(sender) /
		next.disagree(self);
	}

	-- disagree received and forwarded
    RESERVED -> NON_RESERVED {
		disagree(sender) /
		next.disagree(self);
	}

	-- reservation request received
	-- however, the node is already in wait-ack, so it returns a nack to sender
    WAIT_ACK -> WAIT_ACK {
		req(sender, route_index, route_elements, requested_point_positions) /
		sender.nack(self);
	}

	-- reservation request received
	-- however, the node is malfunctioning
    MALFUNCTION -> MALFUNCTION {
		req(sender, route_index, route_elements, requested_point_positions) /
		sender.nack(self);
	}

	-- reservation request received
	-- however, the node is already in wait-commit state and returns nack to the sender
    WAIT_COMMIT -> WAIT_COMMIT {
		req(sender, route_index, route_elements, requested_position) /
		sender.nack(self);
	}

	-- reservation request received
	-- however, the node is already in wait-agree state and returns nack to the sender
    WAIT_AGREE -> WAIT_AGREE {
		req(sender, route_index, route_elements, requested_position) /
		sender.nack(self);
	}

	-- reservation request received
	-- however, the node is already in positioning state and returns nack to the sender
    POSITIONING -> POSITIONING {
		req(sender, route_index, route_elements, requested_position) /
		sender.nack(self);
	}

	-- reservation request received
	-- however, the node is already reserved and returns nack to the sender
    RESERVED -> RESERVED {
		req(sender, route_index, route_elements, requested_position) /
		sender.nack(self);
	}

	-- reservation request received
	-- however, the node is occupied by a train and returns nack to the sender
    TRAIN_IN_TRANSITION -> TRAIN_IN_TRANSITION {
		req(sender, route_index, route_elements, requested_position) /
		sender.nack(self);
	}

end Point
"""
