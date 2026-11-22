/*
 * Copyright 2026 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.clienttest;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.FileOverride;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.Until;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.CompareOptions;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.EntitySpawn;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.Parameters;
import de.markusbordihn.clientruntimeinterfacetoolkit.testrunner.data.api.CaptureCrop;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Set;
import java.util.UUID;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.zip.CRC32;
import java.util.zip.Deflater;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;

class SkinTypeRenderClientTest extends ClientTestBase {

  private static final String NPC_TYPE = "easy_npc:humanoid";
  private static final int PLATFORM_Y = 100;
  private static final int VIEW_Y = PLATFORM_Y + 1;
  private static final long SETTLED_TICKS = 5;
  private static final long SKIN_RENDER_TICKS = 20;
  private static final long PLATFORM_LIGHTING_TICKS = 40;
  private static final EntitySpawn NPC_SPAWN = EntitySpawn.of(NPC_TYPE, 0.5, VIEW_Y, 3.5);
  private static final String DEFAULT_SKIN_DATA = "SkinData:{Type:\"DEFAULT\"}";
  private static final String HIDDEN_NAME_TAG_DATA =
      "DisplayAttribute:[{Type:\"NAME_VISIBILITY\",Text:\"NEVER\"}]";
  private static final double MAXIMUM_DEFAULT_SKIN_CHANGED_RATIO = 0.05;
  private static final double MINIMUM_OTHER_SKIN_CHANGED_RATIO = 0.2;
  private static final double MAXIMUM_HIDDEN_MODEL_CHANGED_RATIO = 0.05;
  private static final CompareOptions MODEL_COMPARE = CompareOptions.DEFAULT.withTolerance(32);
  private static final String HUMANOID_SKIN_FOLDER = "easy_npc/skin/humanoid";
  private static final int SERVER_SKIN_SIZE = 512;
  private static final byte[] SERVER_SKIN_COLOR = {(byte) 255, 0, (byte) 255, (byte) 255};
  private static final int MAXIMUM_SHARED_SKIN_FILE_SIZE = 768 * 1024;
  private static final long SERVER_SKIN_RENDER_TICKS = 60;

  private static String retainEmptyPlatformFrame() {
    awaitWorldSetupCapability();
    placeScene("viewing_platform", -3, PLATFORM_Y, -1);
    client.teleport(0.5, VIEW_Y, 0.5, 0, 0);
    client.await(Until.ticksElapsed(PLATFORM_LIGHTING_TICKS));
    return client.retainFrame();
  }

  private static UUID spawnWithoutNameFacingPlayer(String skinData) {
    UUID npcUUID =
        client.spawnEntity(
            NPC_SPAWN
                .withRotation(180, 0)
                .withNoAi(true)
                .withNbt("{" + HIDDEN_NAME_TAG_DATA + "," + skinData + "}"));
    client.await(
        Until.entityPresent(Parameters.of("type", NPC_TYPE).with("uuid", npcUUID.toString())));
    client.await(Until.settled(SETTLED_TICKS));
    return npcUUID;
  }

  private static CaptureCrop changedBounds(String referenceFrame) {
    return client.compare(referenceFrame, MODEL_COMPARE).comparison().changedBounds().orElseThrow();
  }

  private static double regionChange(String referenceFrame, CaptureCrop npcBounds, long height) {
    return client
        .compare(
            referenceFrame,
            MODEL_COMPARE.withRegion(
                (int) npcBounds.x(),
                (int) npcBounds.y(),
                (int) npcBounds.width(),
                (int) Math.max(1, height)))
        .comparison()
        .changedRatio();
  }

  private static void removeAndAwaitAbsent(UUID npcUUID) {
    client.removeEntity(npcUUID);
    client.await(Until.entityAbsent(Parameters.of("uuid", npcUUID.toString())));
  }

  private static void writeChunk(ByteArrayOutputStream png, String chunkType, byte[] data) {
    byte[] chunkTypeBytes = chunkType.getBytes(StandardCharsets.US_ASCII);
    CRC32 crc = new CRC32();
    crc.update(chunkTypeBytes);
    crc.update(data);
    png.writeBytes(ByteBuffer.allocate(4).putInt(data.length).array());
    png.writeBytes(chunkTypeBytes);
    png.writeBytes(data);
    png.writeBytes(ByteBuffer.allocate(4).putInt((int) crc.getValue()).array());
  }

  private static byte[] uncompressedSolidColorSkin(byte[] rgbaColor) {
    int scanlineLength = 1 + SERVER_SKIN_SIZE * 4;
    byte[] rawImageData = new byte[SERVER_SKIN_SIZE * scanlineLength];
    for (int i = 0; i < rawImageData.length; i++) {
      int scanlinePosition = i % scanlineLength;
      if (scanlinePosition != 0) {
        rawImageData[i] = rgbaColor[(scanlinePosition - 1) % 4];
      }
    }
    Deflater deflater = new Deflater(Deflater.NO_COMPRESSION);
    deflater.setInput(rawImageData);
    deflater.finish();
    ByteArrayOutputStream imageData = new ByteArrayOutputStream();
    byte[] buffer = new byte[65536];
    while (!deflater.finished()) {
      imageData.write(buffer, 0, deflater.deflate(buffer));
    }
    deflater.end();

    ByteArrayOutputStream png = new ByteArrayOutputStream();
    png.writeBytes(new byte[] {(byte) 0x89, 'P', 'N', 'G', '\r', '\n', 0x1A, '\n'});
    writeChunk(
        png,
        "IHDR",
        ByteBuffer.allocate(13)
            .putInt(SERVER_SKIN_SIZE)
            .putInt(SERVER_SKIN_SIZE)
            .put((byte) 8)
            .put((byte) 6)
            .array());
    writeChunk(png, "IDAT", imageData.toByteArray());
    writeChunk(png, "IEND", new byte[0]);
    return png.toByteArray();
  }

  private static Set<String> listHumanoidSkinFiles() {
    Path humanoidSkinFolder = client.configDirectory().resolve(HUMANOID_SKIN_FOLDER);
    if (!Files.isDirectory(humanoidSkinFolder)) {
      return Set.of();
    }
    try (Stream<Path> skinFiles = Files.list(humanoidSkinFolder)) {
      return skinFiles
          .map(skinFile -> skinFile.getFileName().toString())
          .collect(Collectors.toSet());
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private static double serverSkinChange(String caseName, byte[] skinFileContent) {
    String emptyPlatformFrame = retainEmptyPlatformFrame();
    UUID defaultSkinNPCUUID = spawnWithoutNameFacingPlayer(DEFAULT_SKIN_DATA);
    CaptureCrop npcBounds = changedBounds(emptyPlatformFrame);
    String defaultSkinFrame = client.retainFrame();
    removeAndAwaitAbsent(defaultSkinNPCUUID);

    String skinFileName = "clienttest_" + caseName + "_" + UUID.randomUUID() + ".png";
    Set<String> skinFilesBefore = listHumanoidSkinFiles();
    try (FileOverride serverSkinFile =
        client.overrideFile(
            client.configDirectory().resolve(HUMANOID_SKIN_FOLDER).resolve(skinFileName),
            skinFileContent)) {
      client.runCommand("easy_npc reload skins");
      client.await(Until.ticksElapsed(SETTLED_TICKS * 4));
    }

    UUID skinUUID = UUID.nameUUIDFromBytes(skinFileName.getBytes(StandardCharsets.UTF_8));
    UUID testedNPCUUID =
        spawnWithoutNameFacingPlayer(
            "SkinData:{Type:\"CUSTOM\",UUID:[I;"
                + (int) (skinUUID.getMostSignificantBits() >> 32)
                + ","
                + (int) skinUUID.getMostSignificantBits()
                + ","
                + (int) (skinUUID.getLeastSignificantBits() >> 32)
                + ","
                + (int) skinUUID.getLeastSignificantBits()
                + "]}");
    client.await(Until.ticksElapsed(SERVER_SKIN_RENDER_TICKS));
    double skinChange = regionChange(defaultSkinFrame, npcBounds, npcBounds.height());
    client.saveScreenshot(caseName);
    removeAndAwaitAbsent(testedNPCUUID);
    client.runCommand("easy_npc reload skins");

    assertEquals(
        skinFilesBefore, listHumanoidSkinFiles(), "Server skins must never be written to disk");
    return skinChange;
  }

  @Test
  @DisplayName("A poorly compressed server skin missing on the client renders after download")
  void serverSkinRendersAfterDownload() {
    byte[] uncompressedSkin = uncompressedSolidColorSkin(SERVER_SKIN_COLOR);
    assertTrue(uncompressedSkin.length > MAXIMUM_SHARED_SKIN_FILE_SIZE);

    double skinChange = serverSkinChange("server_skin", uncompressedSkin);

    assertTrue(
        skinChange >= MINIMUM_OTHER_SKIN_CHANGED_RATIO,
        "Server skin differs from the default skin in only " + skinChange);
  }

  @Test
  @DisplayName("A server skin with data after IEND is rejected and renders the default skin")
  void rejectedServerSkinRendersDefaultSkin() {
    ByteArrayOutputStream polyglotSkinFile = new ByteArrayOutputStream();
    polyglotSkinFile.writeBytes(uncompressedSolidColorSkin(SERVER_SKIN_COLOR));
    polyglotSkinFile.writeBytes(new byte[] {'P', 'K', 3, 4});

    double skinChange = serverSkinChange("rejected_server_skin", polyglotSkinFile.toByteArray());

    assertTrue(
        skinChange <= MAXIMUM_DEFAULT_SKIN_CHANGED_RATIO,
        "Rejected server skin differs from the default skin in " + skinChange);
  }

  @ParameterizedTest(name = "{0}")
  @CsvSource(
      delimiter = '|',
      value = {
        "player_skin_without_uuid | SkinData:{Type:\"PLAYER_SKIN\",Name:\"Steve\"}",
        "player_skin_with_string_uuid | SkinData:{Type:\"PLAYER_SKIN\",Name:\"Steve\","
            + "UUID:\"8667ba71-b85a-4004-af54-457a9734eed7\"}",
        "custom_skin_without_uuid | SkinData:{Type:\"CUSTOM\"}",
        "custom_skin_with_short_uuid | SkinData:{Type:\"CUSTOM\",UUID:[I;1,2]}",
        "secure_remote_skin_with_string_uuid | SkinData:{Type:\"SECURE_REMOTE_URL\","
            + "URL:\"https://example.com/skin.png\",UUID:\"8667ba71-b85a-4004-af54-457a9734eed7\"}",
        "insecure_remote_skin_without_uuid | SkinData:{Type:\"INSECURE_REMOTE_URL\","
            + "URL:\"http://example.com/skin.png\"}"
      })
  @DisplayName("Skins without a usable UUID spawn and render the default skin")
  void skinsWithoutUsableUUIDRenderDefaultSkin(String caseName, String skinData) {
    String emptyPlatformFrame = retainEmptyPlatformFrame();
    UUID defaultSkinNPCUUID = spawnWithoutNameFacingPlayer(DEFAULT_SKIN_DATA);
    CaptureCrop npcBounds = changedBounds(emptyPlatformFrame);
    String defaultSkinFrame = client.retainFrame();
    removeAndAwaitAbsent(defaultSkinNPCUUID);

    UUID testedNPCUUID = spawnWithoutNameFacingPlayer(skinData);
    client.await(Until.ticksElapsed(SKIN_RENDER_TICKS));
    double skinChange = regionChange(defaultSkinFrame, npcBounds, npcBounds.height());
    client.saveScreenshot(caseName);
    removeAndAwaitAbsent(testedNPCUUID);

    assertTrue(
        skinChange <= MAXIMUM_DEFAULT_SKIN_CHANGED_RATIO,
        caseName + " differs from the default skin in " + skinChange + " of the NPC area");
  }

  @Test
  @DisplayName("A resource location skin renders its own texture instead of the default skin")
  void resourceLocationSkinRendersOwnTexture() {
    String emptyPlatformFrame = retainEmptyPlatformFrame();
    UUID defaultSkinNPCUUID = spawnWithoutNameFacingPlayer(DEFAULT_SKIN_DATA);
    CaptureCrop npcBounds = changedBounds(emptyPlatformFrame);
    String defaultSkinFrame = client.retainFrame();
    removeAndAwaitAbsent(defaultSkinNPCUUID);

    UUID testedNPCUUID =
        spawnWithoutNameFacingPlayer(
            "SkinData:{Type:\"RESOURCE_LOCATION\","
                + "Texture:\"easy_npc:textures/entity/humanoid/humanoid_template.png\"}");
    client.await(Until.ticksElapsed(SKIN_RENDER_TICKS));
    double skinChange = regionChange(defaultSkinFrame, npcBounds, npcBounds.height());
    client.saveScreenshot("resource_location_skin");
    removeAndAwaitAbsent(testedNPCUUID);

    assertTrue(
        skinChange >= MINIMUM_OTHER_SKIN_CHANGED_RATIO,
        "Resource location skin differs from the default skin in only " + skinChange);
  }

  @Test
  @DisplayName("A skin of type NONE renders no visible model")
  void noneSkinRendersNoVisibleModel() {
    String emptyPlatformFrame = retainEmptyPlatformFrame();
    UUID defaultSkinNPCUUID = spawnWithoutNameFacingPlayer(DEFAULT_SKIN_DATA);
    CaptureCrop npcBounds = changedBounds(emptyPlatformFrame);
    removeAndAwaitAbsent(defaultSkinNPCUUID);

    UUID testedNPCUUID = spawnWithoutNameFacingPlayer("SkinData:{Type:\"NONE\"}");
    client.await(Until.ticksElapsed(SKIN_RENDER_TICKS));
    double modelChange = regionChange(emptyPlatformFrame, npcBounds, npcBounds.height() / 2);
    client.saveScreenshot("none_skin");
    removeAndAwaitAbsent(testedNPCUUID);

    assertTrue(
        modelChange <= MAXIMUM_HIDDEN_MODEL_CHANGED_RATIO,
        "NONE skin still renders " + modelChange + " of the upper NPC area");
  }
}
