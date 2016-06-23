package org.dbpedia.extraction.live.feeder;

import com.google.gson.JsonObject;
import io.socket.IOAcknowledge;
import io.socket.IOCallback;
import io.socket.SocketIO;
import io.socket.SocketIOException;
import org.dbpedia.extraction.live.main.Main;
import org.dbpedia.extraction.live.queue.LiveQueueItem;
import org.dbpedia.extraction.live.queue.LiveQueuePriority;
import org.dbpedia.extraction.live.util.DateUtil;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collection;

/**
 * This class extends the default Feeder for RCStreem handling.
 * It registers at given socket and listens messages.
 * Those messages indicate changes in a specified mediawiki.
 *
 * @author Lukas Faber, Stephan Haarmann, Sebastian Serth
 * date 07.05.2016.
 */
public class RCStreamFeeder extends Feeder implements IOCallback {

    /** The Socket used for receiving the RCStream */
    private static String WIKIMEDIA_RCSTREAM_URL = "http://stream.wikimedia.org/rc";

    private SocketIO socket;
    /** The room describes the wiki, which RCStream will be processed e.G. https://en.wikipedia.org */
    private String room;
    private Collection<LiveQueueItem> events;

    private static final Logger logger = LoggerFactory.getLogger(Main.class);

    public RCStreamFeeder(String feederName, LiveQueuePriority queuePriority, String defaultStartTime,
                          String folderBasePath, String room) {
        super(feederName, queuePriority, defaultStartTime, folderBasePath);
        this.room = room;
        try {
            connect();
        } catch(MalformedURLException e){

        }
        events = new ArrayList<LiveQueueItem>();
    }

    @Override
    protected void initFeeder() {
        // do nothing
    }

    /**
     * Connects to a mediawiki RCstream (e.G. http://stream.wikimedia.org/rc).
     * A MalformedURLException is raised if the URL is wrong.
     *
     * @throws MalformedURLException Connection to socket could not be established.
     */
    protected void connect() throws MalformedURLException {
        socket = new SocketIO(WIKIMEDIA_RCSTREAM_URL);
        socket.connect(this);
    }

    @Override
    protected Collection<LiveQueueItem> getNextItems() {
        Collection<LiveQueueItem> returnValue;
        synchronized (this){
            returnValue = events;
            events = new ArrayList<LiveQueueItem>();
        }
        return returnValue;
    }

    @Override
    public void onDisconnect() {
        try {
            connect();
        } catch (MalformedURLException e) {
            e.printStackTrace();
        }
    }

    @Override
    public void onConnect() {
        socket.emit("subscribe", room);
    }

    @Override
    public void onMessage(String data, IOAcknowledge ack) {
        logger.debug("Message: " + data);
    }

    @Override
    public void onMessage(com.google.gson.JsonElement json, IOAcknowledge ack) {
        logger.debug("Message: " + json.toString());
    }

    @Override
    public void on(String event, IOAcknowledge ack, com.google.gson.JsonElement... args) {
        JsonObject jsonObject = (JsonObject) args[0];
        String title = jsonObject.get("title").getAsString();
        Long timestamp = jsonObject.get("timestamp").getAsLong();
        String eventTimestamp = DateUtil.transformToUTC(timestamp * 1000L);
        synchronized (this){
            events.add(new LiveQueueItem(-1, title, eventTimestamp, false, ""));
            logger.debug("Registered event for page " + title + " at " + eventTimestamp);
        }
    }

    /**
     * Logs exceptions, that occur while listing to the RCStream.
     *
     * @param socketIOException The exception thrown by the socket connection.
     */
    @Override
    public void onError(SocketIOException socketIOException) {
        logger.error("An error in the RCStream connection occured: " + socketIOException.getMessage());
    }
}
