package com.example.runtimeproto;

import android.app.IntentService;
import android.content.Context;
import android.content.Intent;
import android.widget.TextView;

import java.util.ArrayDeque;
import java.util.Deque;

import jp.ne.sakura.uhideyuki.brt.runtime.RT;
import com.example.runtimeproto.jout.*;

/**
 * An {@link IntentService} subclass for handling asynchronous task requests in
 * a service on a separate handler thread.
 * <p>
 * TODO: Customize class - update intent actions, extra parameters and static
 * helper methods.
 */
public class MainIntentService extends IntentService {
    private static Deque<String> dequeIn = new ArrayDeque<>();
    private static String buf = null;
    public static TextView textView;

    public MainIntentService() {
        super("MainIntentService");
    }

    /**
     * Starts this service to perform action Main.
     * If the service is already performing a task this action will be queued.
     *
     * @see IntentService
     */
    public static void startMain(Context context) {
        Intent intent = new Intent(context, MainIntentService.class);
        context.startService(intent);
    }

    public static void setTextview(TextView tv){
        textView = tv;
    }

    @Override
    protected void onHandleIntent(Intent intent) {
        handleActionMain();
    }

    /**
     * Handle action Main in the provided background thread.
     */
    private void handleActionMain() {
        RT.eval(Main.mkmain());
    }

    public static void putline(String s){
        synchronized (dequeIn) {
            dequeIn.addLast(s);
        }
    }

    public static char getChar(){
        char c;
        if (buf == null){
            while (buf == null){
                synchronized(dequeIn) {
                    if (dequeIn.size() > 0){
                        buf = dequeIn.removeFirst();
                    }
                }
            }
        }
        if (buf.isEmpty()){
            c = '\n';
            buf = null;
        } else {
            c = buf.charAt(0);
            buf = buf.substring(1);
        }
        return c;
    }

    public static void putStrLn(String str){
        synchronized (textView) {
            textView.append(str + "\n");
        }
    }
}
