// I2C
#include <Wire.h>
TwoWire myWire = TwoWire(0);

// Pressure Sensor
#include <DFRobot_BMP3XX.h>
DFRobot_BMP388_I2C sensor(&myWire, sensor.eSDOVDD);

// MPU
#include <Adafruit_MPU6050.h>
#include <Adafruit_Sensor.h>
Adafruit_MPU6050 mpu;

#ifdef ERR_OK
#undef ERR_OK
#endif

// Sd Card
#include "FS.h"
#include "SD.h"
#include "SPI.h"
#include <EEPROM.h>
#define TRACK_NUMBER_EEPROM_SIZE 4
int trackNumber = 0;
String fileName;

// Status LED
#define PIN_RED    32
#define PIN_GREEN  33
#define PIN_BLUE   4
#define PWM1_Ch    0
#define PWM1_Res   12
#define PWM1_Freq  5000
int brightness = 256;

// GPS
#include <TinyGPS++.h>
#include <SoftwareSerial.h>
static const int RXPin = 16, TXPin = 17;
static const uint32_t GPSBaud = 38400;
TinyGPSPlus gps;
SoftwareSerial ss(RXPin, TXPin);

// WebServer
#include <WiFi.h>
#include <ESPAsyncWebServer.h>
const char* apSSID = "Rocket";
const char* apPassword = ""; // No password
AsyncWebServer server(80);

// Global variables for sensor data
float temperature = 0.0;
float pressure = 0.0;
float altitude = 0.0;
float lat = 0.0;
float lng = 0.0;
float hdop = 0.0;
int gpsCount = 0;
sensors_event_t a, g, temp;

void setup() {
  pinMode(PIN_RED,   OUTPUT);
  pinMode(PIN_BLUE,   OUTPUT);
  digitalWrite(PIN_RED, HIGH);
  digitalWrite(PIN_BLUE, LOW);

  ledcSetup(PWM1_Ch, PWM1_Freq, PWM1_Res);
  ledcAttachPin(PIN_GREEN, PWM1_Ch);

  Serial.begin(115200);
  myWire.begin(0, 4);

  if (!initializeSd()
      || !initializeEeprom()
      || !initializePressureSensor()
      || !initializeMpu())
  {
    digitalWrite(PIN_RED, HIGH);
    while (1)
    {
      delay(100);
    }
  }

  ss.begin(GPSBaud);

  digitalWrite(PIN_RED, LOW);
  printSdInformation();
  readTrackNumber();

  setupServer();
}

void setupServer()
{
  // Set up the ESP32 as an access point
  WiFi.softAP(apSSID, apPassword);
  Serial.println("Access Point Started");
  IPAddress IP = WiFi.softAPIP();
  Serial.print("AP IP Address: ");
  Serial.println(IP);

  // Define the route for the web server
  server.on("/", HTTP_GET, [](AsyncWebServerRequest * request) {
    String html = "<html><head><meta charset='UTF-8'><meta name='viewport' content='width=device-width, initial-scale=1'><style>"
                  "body { font-family: Arial, sans-serif; margin: 0; padding: 0; background-color: #f0f0f0; }"
                  ".container { display: flex; flex-direction: column; align-items: center; padding: 20px; }"
                  ".section { margin: 20px; padding: 20px; background: white; border-radius: 8px; box-shadow: 0 0 10px rgba(0,0,0,0.1); width: 100%; max-width: 600px; }"
                  "h1 { font-size: 2em; margin-bottom: 10px; }"
                  "p { font-size: 1.2em; margin: 5px 0; }"
                  "@media (max-width: 600px) { h1 { font-size: 1.5em; } p { font-size: 1em; } }"
                  "</style></head><body><div class='container'>"
                  "<div class='section'><h1>GPS</h1>"
                  "<p>Lat: " + String(lat, 6) + "</p>"
                  "<p>Lng: " + String(lng, 6) + "</p>"
                  "<p>Hdop: " + String(hdop) + "</p>"
                  "<p>Satellites: " + String(gpsCount) + "</p></div>"
                  "<div class='section'><h1>Barometer</h1>"
                  "<p>Pressure: " + String(pressure) + " Pa</p>"
                  "<p>Altitude: " + String(altitude) + " m</p>"
                  "<p>Temperature: " + String(temperature) + " °C</p></div>"
                  "<div class='section'><h1>Gyro</h1>"
                  "<p>Acceleration X: " + String(a.acceleration.x) + " m/s²</p>"
                  "<p>Acceleration Y: " + String(a.acceleration.y) + " m/s²</p>"
                  "<p>Acceleration Z: " + String(a.acceleration.z) + " m/s²</p>"
                  "<p>Gyro X: " + String(g.gyro.x) + " °/s</p>"
                  "<p>Gyro Y: " + String(g.gyro.y) + " °/s</p>"
                  "<p>Gyro Z: " + String(g.gyro.z) + " °/s</p></div>"
                  "<script>setTimeout(function(){ location.reload(); }, 1000);</script></body></html>";
    request->send(200, "text/html", html);
  });

  // Start the server
  server.begin();
}

bool initializeEeprom()
{
  if (!EEPROM.begin(TRACK_NUMBER_EEPROM_SIZE)) {
    Serial.println("Failed to initialise EEPROM");
    return false;
  }
  return true;
}

bool initializeSd()
{
  if (!SD.begin(5)) {
    Serial.println("Card Mount Failed");
    return false;
  }

  uint8_t cardType = SD.cardType();

  if (cardType == CARD_NONE) {
    Serial.println("No SD card attached");
    return false;
  }

  return true;
}

bool initializePressureSensor()
{
  int rslt;
  while ( ERR_OK != (rslt = sensor.begin()) ) {
    if (ERR_DATA_BUS == rslt) {
      Serial.println("Data bus error!!!");
      return false;
    } else if (ERR_IC_VERSION == rslt) {
      Serial.println("Chip versions do not match!!!");
      return false;
    }
    delay(3000);
  }
  while ( !sensor.setSamplingMode(sensor.eNormalPrecision2) ) {
    Serial.println("Set samping mode fail, retrying....");
    delay(3000);
  }
  Serial.println("Pressure Sensor running!");

  /* Get the sampling period of the current measurement mode, unit: us */
  float sampingPeriodus = sensor.getSamplingPeriodUS();
  Serial.print("samping period : ");
  Serial.print(sampingPeriodus);
  Serial.println(" us");

  /* Get the sampling frequency of the current measurement mode, unit: Hz */
  float sampingFrequencyHz = 1000000 / sampingPeriodus;
  Serial.print("samping frequency : ");
  Serial.print(sampingFrequencyHz);
  Serial.println(" Hz");
  return true;
}

bool initializeMpu()
{
  if (!mpu.begin(MPU6050_I2CADDR_DEFAULT, &myWire)) {
    Serial.println("Failed to find MPU6050 chip");
    return false;
  }

  mpu.setAccelerometerRange(MPU6050_RANGE_16_G);
  mpu.setGyroRange(MPU6050_RANGE_500_DEG);
  mpu.setFilterBandwidth(MPU6050_BAND_44_HZ);
  return true;
}

void printSdInformation()
{
  Serial.print("SD Card Type: ");
  uint8_t cardType = SD.cardType();
  if (cardType == CARD_MMC) {
    Serial.println("MMC");
  } else if (cardType == CARD_SD) {
    Serial.println("SDSC");
  } else if (cardType == CARD_SDHC) {
    Serial.println("SDHC");
  } else {
    Serial.println("UNKNOWN");
  }

  uint64_t cardSize = SD.cardSize() / (1024 * 1024);
  Serial.printf("SD Card Size: %lluMB\n", cardSize);
}

void readTrackNumber()
{
  int trackNumber = 0;

  // Read the current value stored in EEPROM
  EEPROM.get(0, trackNumber);

  // Print the current value to the serial monitor
  Serial.print("Current track number: ");
  Serial.println(trackNumber);

  // If trackNumber is invalid, initialize it
  if (trackNumber < 0) {
    Serial.println("EEPROM is uninitialized or invalid. Setting trackNumber to 0.");
    trackNumber = 0;
  }

  // Increment the value
  trackNumber++;

  // Write the new value back to EEPROM
  EEPROM.put(0, trackNumber);
  EEPROM.commit();

  // Print the new value to the serial monitor
  Serial.print("New track number: ");
  Serial.println(trackNumber);

  fileName = "/" + String(trackNumber) + ".csv";
  Serial.print("Log file: ");
  Serial.println(fileName);
}

void writeFile(fs::FS &fs, String path, const char *message) {

  File file = fs.open(path, FILE_APPEND);
  if (!file) {
    Serial.println("Failed to open file for writing");
    return;
  }
  if (!file.print(message)) {
    Serial.println("Line Write failed");
  }
  file.print("\n");
  file.close();
}

void heartbeatLed()
{
  if (brightness > 1)
    brightness -= 20;
  else
    brightness = 256;

  ledcWrite(PWM1_Ch, brightness);
}

// This custom version of delay() ensures that the gps object
// is being "fed".
static void smartDelay(unsigned long ms)
{
  unsigned long start = millis();
  do
  {
    while (ss.available())
      gps.encode(ss.read());
  } while (millis() - start < ms);
}

void loop() {
  heartbeatLed();

  unsigned long startTime = millis();

  // Pressure
  temperature = sensor.readTempC();
  pressure = sensor.readPressPa();
  altitude = sensor.readAltitudeM();

  // MPU
  mpu.getEvent(&a, &g, &temp);

  // GPS
  if (millis() > 5000 && gps.charsProcessed() < 10) {
    Serial.println(F("No GPS data received: check wiring"));
    digitalWrite(PIN_BLUE, LOW);
  }

  if (gps.satellites.isValid())
    gpsCount = gps.satellites.value();

  if (gps.hdop.isValid())
    hdop = gps.hdop.hdop();

  float gpsAltitude = 0;
  if (gps.location.isValid()) {
    lat = gps.location.lat();
    lng = gps.location.lng();
    gpsAltitude = gps.altitude.meters();
    digitalWrite(PIN_BLUE, HIGH);
  }

  char buffer[400];
  int len = snprintf(buffer, sizeof(buffer), "%lu,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.6f,%.6f,%.2f,%.2f,%.2f,%02d:%02d:%02d.%02d",
                     millis(),
                     temperature, pressure, altitude,
                     a.acceleration.x, a.acceleration.y, a.acceleration.z, g.gyro.x, g.gyro.y, g.gyro.z,
                     gpsCount, hdop, lat, lng, gpsAltitude, gps.course.deg(), gps.speed.kmph(), gps.time.hour(), gps.time.minute(), gps.time.second(), gps.time.centisecond());

  if (len < 0 || len >= sizeof(buffer)) {
    Serial.println("Buffer overflow or encoding error");
  } else {
    writeFile(SD, fileName, buffer);
  }

  unsigned long endTime = millis();
  unsigned long elapsedTime = endTime - startTime;

  unsigned long delayTime = (elapsedTime >= 60) ? 0 : (60 - elapsedTime);
  smartDelay(delayTime);
}
