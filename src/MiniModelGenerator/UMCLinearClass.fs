module UMCLinear

let class_definition = """
Class Linear is

  Signals:
	req(sender: obj, route_index: int, route_elements: obj[],
		requested_point_positions: bool[]);
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
    train: obj := null;

  State Top = NON_RESERVED, WAIT_ACK, WAIT_COMMIT, WAIT_AGREE, RESERVED, TRAIN_IN_TRANSITION

  Transitions

	-- first node receive request
    NON_RESERVED -> WAIT_ACK {
		req(sender, route_index, route_elements, requested_point_positions)
		[route_index = 0 and sender = train and route_elements.length > 0] /
		prev := null;
		next := route_elements[1];
		next.req(self, 1, route_elements, requested_point_positions);
	}

	-- intermediate node receive request
    NON_RESERVED -> WAIT_ACK {
		req(sender, route_index, route_elements, requested_point_positions)
		[train = null and (route_index > 0 and route_index+1 < route_elements.length)] /
		prev := route_elements[route_index - 1];
		next := route_elements[route_index + 1];
		next.req(self, route_index + 1, route_elements, requested_point_positions);
	}

	-- initial reservation request for last node
	-- starts ack phase
    NON_RESERVED -> WAIT_COMMIT {
		req(sender, route_index, route_elements, requested_point_positions)
		[train = null and route_elements.length = route_index+1] /
		prev := route_elements[route_index - 1];
		next := null;
		prev.ack(self);
	}

	-- intermediate node receive ack
    WAIT_ACK -> WAIT_COMMIT {
		ack(sender)
		[prev /= null] /
		prev.ack(self);
	}

	-- first node receive ack
	-- and starts commit phase
    WAIT_ACK -> WAIT_AGREE {
		ack(sender)
		[prev = null] /
		next.commit(self);
	}

	-- intermediate node receives commit
    WAIT_COMMIT -> WAIT_AGREE {
		commit(sender)
		[next /= null] /
		next.commit(self);
	}

	-- last node receive commit
	-- and starts agree phase
    WAIT_COMMIT -> RESERVED {
		commit(sender)
		[next = null] /
		prev.agree(self);
	}

	-- intermediate node receive agree
    WAIT_AGREE -> RESERVED {
		agree(sender)
		[prev /= null] /
		prev.agree(self);
	}

	-- first node receive agree
	-- and sends ok to the train
    WAIT_AGREE -> TRAIN_IN_TRANSITION {
		agree(sender)
		[prev = null and train /= null] /
		train.ok;
	}

	-- train moves onto current node
    RESERVED -> TRAIN_IN_TRANSITION {
		sensorOn(sender) /
		train := sender;
	}

	-- sequential release
	-- reset train
    TRAIN_IN_TRANSITION -> NON_RESERVED {
		sensorOff(sender) /
		train := null;
	}

	-- nack received
	-- forwards and goes into non-reserved
    WAIT_ACK -> NON_RESERVED {
		nack(sender) /
		if  prev = null  then { -- is first node on itinerary
			train.no
		} else { -- is not first node
			prev.nack(self)
		};
	}

	-- disagree received
	-- forwards and goes into non-reserved
    WAIT_COMMIT -> NON_RESERVED {
		disagree(sender) /
		if next /= null  then { -- not last node on itinerary
			next.disagree(self)
		};
	}

	-- disagree received
	-- forwards and goes into non-reserved
    WAIT_AGREE -> NON_RESERVED {
		disagree(sender) /
		if prev /= null then { -- not first node on itinerary
			prev.disagree(self)
		} else { -- is first node
			train.no
		};
	}

	-- disagree received
	-- forwards (if there is someone to forward to) and goes into non-reserved
    RESERVED -> NON_RESERVED {
		disagree(sender) /
		if next /= null then { -- not last node on itinerary
			next.disagree(self)
		};
	}

	-- reservation request received
	-- however a train is already on the track, so a nack is returned to sender
    NON_RESERVED -> NON_RESERVED {
		req(sender, route_index, route_elements, requested_point_positions)
		[train /= null and sender /= train] /
		sender.nack(self);
	}

	-- reservation request received
	-- however, the node is already in wait-ack, so it returns a nack to sender
    WAIT_ACK -> WAIT_ACK {
		req(sender, route_index, route_elements, requested_point_positions) /
		sender.nack(self);
	}

	-- reservation request received
	-- however, the node is already in wait-commit, so it returns a nack to sender
    WAIT_COMMIT -> WAIT_COMMIT {
		req(sender, route_index, route_elements, requested_point_positions) /
		sender.nack(self);
	}

	-- reservation request received
	-- however, the node is already in wait-agree, so it returns a nack to sender
    WAIT_AGREE -> WAIT_AGREE {
		req(sender, route_index, route_elements, requested_point_positions) /
		sender.nack(self);
	}

	-- reservation request received
	-- however, the node is already reserved, so it returns a nack to sender
    RESERVED -> RESERVED {
		req(sender, route_index, route_elements, requested_point_positions) /
		sender.nack(self);
	}

	-- reservation request received
	-- however, a train is in transition on the node, so it returns a nack to sender
    TRAIN_IN_TRANSITION -> TRAIN_IN_TRANSITION {
		req(sender, route_index, route_elements, requested_point_positions) /
		sender.nack(self);
	}

end Linear
"""
