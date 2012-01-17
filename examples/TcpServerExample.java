import java.util.Date;

import org.hurricane.Gateway;
import org.hurricane.Message;

public class TcpServerExample {
    public static void main(String[] args) throws Exception {
        Gateway gateway = new Gateway("localhost", 3000);
        gateway.registerServer("time_server");

        while (true) {
            Message request = gateway.recv();
            System.out.println(request);

            Message response = new Message();
            response.setType("response");
            response.setDestination(request.getDestination());
            response.setTag(request.getTag());
            response.setData(new Date().toString());

            System.out.println(response);
            gateway.send(response);
        }
    }
}
