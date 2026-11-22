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

package de.markusbordihn.easynpc.gametest;

import de.markusbordihn.easynpc.data.action.ActionContext;
import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.data.skin.SkinModel;
import de.markusbordihn.easynpc.data.skin.SkinType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPCBase;
import de.markusbordihn.easynpc.handler.SkinHandler;
import de.markusbordihn.easynpc.io.DataFileHandler;
import de.markusbordihn.easynpc.io.ServerSkinIndex;
import de.markusbordihn.easynpc.io.ServerSkinIndex.ServerSkin;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.UncheckedIOException;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.UUID;
import java.util.zip.CRC32;
import java.util.zip.Deflater;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class SkinTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);

  private SkinTestHelper() {}

  private static int executeConsoleCommand(GameTestHelper helper, String command) {
    return GameTestHelpers.executeCommand(
        helper, GameTestHelpers.consoleCommandSource(helper, NPC_POSITION), command);
  }

  private static void setSkinByAction(EasyNPC<?> easyNPC, String skinReference) {
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(new ActionDataEntry(ActionDataType.SET_SKIN, skinReference));
    easyNPC
        .getEasyNPCActionHandler()
        .executeActions(actionDataSet, ActionContext.of(ActionEventType.ON_SPAWN, null, null));
  }

  private static SkinDataEntry getSkin(EasyNPC<?> easyNPC) {
    return easyNPC.getEasyNPCSkinData().getSkinDataEntry();
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

  private static byte[] createUncompressedSkin(int width, int height) {
    Deflater deflater = new Deflater(Deflater.NO_COMPRESSION);
    deflater.setInput(new byte[height * (1 + width * 4)]);
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
        ByteBuffer.allocate(13).putInt(width).putInt(height).put((byte) 8).put((byte) 6).array());
    writeChunk(png, "IDAT", imageData.toByteArray());
    writeChunk(png, "IEND", new byte[0]);
    return png.toByteArray();
  }

  private static void writeSkinFile(Path skinFile, byte[] pngBytes) {
    try {
      Files.createDirectories(skinFile.getParent());
      Files.write(skinFile, pngBytes);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  private static void deleteSkinFile(Path skinFile) {
    try {
      Files.deleteIfExists(skinFile);
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    }
  }

  public static void assertServerSkinIsRecompressedAndSelectable(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();
    SkinModel skinModel = easyNPC.getEasyNPCSkinData().getSkinModel();
    SkinModel otherSkinModel =
        skinModel == SkinModel.HUMANOID ? SkinModel.HUMANOID_SLIM : SkinModel.HUMANOID;
    String skinName = "Gametest Skin " + UUID.randomUUID().toString().substring(0, 8);
    String otherModelSkinName = skinName + " Other Model";
    Path skinFolder = DataFileHandler.getCustomDataFolder().resolve("skin");
    Path skinFile = skinFolder.resolve(skinModel.getName()).resolve(skinName + ".png");
    Path otherModelSkinFile =
        skinFolder.resolve(otherSkinModel.getName()).resolve(otherModelSkinName + ".png");
    byte[] uncompressedSkin = createUncompressedSkin(512, 512);
    try {
      writeSkinFile(skinFile, uncompressedSkin);
      writeSkinFile(otherModelSkinFile, uncompressedSkin);
      executeConsoleCommand(helper, "easy_npc reload skins");

      ServerSkin serverSkin = ServerSkinIndex.findSkins(skinName).get(0);
      GameTestHelpers.assertTrue(
          helper,
          "Uncompressed skin above the share limit must be recompressed",
          uncompressedSkin.length > ServerSkinIndex.MAX_SKIN_FILE_SIZE
              && serverSkin.pngBytes().length < ServerSkinIndex.MAX_SKIN_FILE_SIZE);
      GameTestHelpers.assertTrue(
          helper,
          "Skin file on disk must stay unchanged",
          Arrays.equals(uncompressedSkin, Files.readAllBytes(skinFile)));

      int setResult =
          executeConsoleCommand(
              helper, "easy_npc skin set custom " + easyNPC.getEntityUUID() + " " + skinName);
      GameTestHelpers.assertEquals(helper, "Skin set custom server skin result", 1, setResult);
      GameTestHelpers.assertEquals(
          helper,
          "Skin UUID after set server skin",
          serverSkin.skinUUID(),
          getSkin(easyNPC).uuid());

      int otherModelResult =
          executeConsoleCommand(
              helper,
              "easy_npc skin set custom " + easyNPC.getEntityUUID() + " " + otherModelSkinName);
      GameTestHelpers.assertEquals(
          helper, "Skin of another skin model must be rejected", 0, otherModelResult);
      GameTestHelpers.assertEquals(
          helper,
          "Rejected skin keeps the current skin",
          serverSkin.skinUUID(),
          getSkin(easyNPC).uuid());
    } catch (IOException e) {
      throw new UncheckedIOException(e);
    } finally {
      deleteSkinFile(skinFile);
      deleteSkinFile(otherModelSkinFile);
      ServerSkinIndex.reload();
    }
  }

  public static void assertSetCustomSkinCommandKeepsLayers(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();
    executeConsoleCommand(helper, "easy_npc skin layer " + easyNPC.getEntityUUID() + " disable");

    int fileNameResult =
        executeConsoleCommand(
            helper, "easy_npc skin set custom " + easyNPC.getEntityUUID() + " Town Guard");

    GameTestHelpers.assertEquals(helper, "Skin set custom by name result", 1, fileNameResult);
    GameTestHelpers.assertEquals(
        helper, "Skin type after set by name", SkinType.CUSTOM, getSkin(easyNPC).type());
    GameTestHelpers.assertEquals(
        helper,
        "Skin UUID after set by name",
        UUID.nameUUIDFromBytes("Town Guard.png".getBytes(StandardCharsets.UTF_8)),
        getSkin(easyNPC).uuid());
    GameTestHelpers.assertTrue(
        helper, "Disabled layers must stay disabled", getSkin(easyNPC).disableLayers());

    UUID skinUUID = UUID.randomUUID();
    int uuidResult =
        executeConsoleCommand(
            helper, "easy_npc skin set custom " + easyNPC.getEntityUUID() + " " + skinUUID);

    GameTestHelpers.assertEquals(helper, "Skin set custom by UUID result", 1, uuidResult);
    GameTestHelpers.assertEquals(
        helper, "Skin UUID after set by UUID", skinUUID, getSkin(easyNPC).uuid());
  }

  public static void assertSetSkinActionChangesSkin(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();
    SkinHandler.setSkin(easyNPC, getSkin(easyNPC).withDisableLayers(true));

    UUID skinUUID = UUID.randomUUID();
    setSkinByAction(easyNPC, skinUUID.toString());

    GameTestHelpers.assertEquals(
        helper, "Skin type after custom skin action", SkinType.CUSTOM, getSkin(easyNPC).type());
    GameTestHelpers.assertEquals(
        helper, "Skin UUID after custom skin action", skinUUID, getSkin(easyNPC).uuid());
    GameTestHelpers.assertTrue(
        helper, "Skin action keeps disabled layers", getSkin(easyNPC).disableLayers());

    Enum<?>[] variants = ((EasyNPCBase<?>) easyNPC).getSkinVariantTypes();
    String variant = variants[variants.length - 1].name();
    setSkinByAction(easyNPC, variant);

    GameTestHelpers.assertEquals(
        helper, "Skin type after variant action", SkinType.DEFAULT, getSkin(easyNPC).type());
    GameTestHelpers.assertEquals(
        helper, "Skin variant after variant action", variant, getSkin(easyNPC).name());

    setSkinByAction(easyNPC, "NOT_A_VARIANT");

    GameTestHelpers.assertEquals(
        helper, "Unknown skin keeps the current skin", variant, getSkin(easyNPC).name());
  }
}
