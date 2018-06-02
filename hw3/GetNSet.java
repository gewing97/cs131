import java.util.concurrent.atomic.AtomicIntegerArray;

class GetNSet implements State {
    private AtomicIntegerArray value;
    private byte maxval;

    GetNSet(byte[] v) { 
        int len = v.length;
        int [] temp = new int[len];
        for(int i = 0; i < len; i++){
            temp[i] = (int) v[i];
        }
        value = new AtomicIntegerArray(temp); 
        maxval = 127; 
    }

    GetNSet(byte[] v, byte m) { 
        int len = v.length;
        int [] temp = new int[len];
        for(int i = 0; i < len; i++){
            temp[i] = (int) v[i];
        }
        value = new AtomicIntegerArray(temp); 
        maxval = m;
    }

    public int size() { return value.length(); }

    public byte[] current() {
        int len = this.size();
        byte [] temp = new byte[len]; 
        for(int i = 0; i < len; i++){
            temp[i] = (byte) value.get(i);
        }
        return temp; 
    }

    public boolean swap(int i, int j) {
        if(value.get(i) <= 0 || value.get(j) >= maxval){
            return false;
        }
        value.getAndDecrement(i);
        value.getAndIncrement(j);
        return true;
    }
}
