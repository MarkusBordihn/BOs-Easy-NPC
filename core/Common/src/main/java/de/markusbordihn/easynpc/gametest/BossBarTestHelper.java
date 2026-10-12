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

import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.handlers.BossBarHandler;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.BossEvent;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class BossBarTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 SECOND_NPC_POSITION = new Vec3(1, 2, 2);
  private static final Vec3 PLAYER_POSITION = new Vec3(1, 2, 0);
  private static final int PRESET_RANGE = 48;

  private BossBarTestHelper() {}

  private static BossBarHandler tickBossBar(EasyNPC<?> easyNPC) {
    BossBarHandler bossBarHandler = new BossBarHandler();
    easyNPC.getLivingEntity().tickCount = 0;
    bossBarHandler.tick(easyNPC);
    return bossBarHandler;
  }

  public static void assertBossBarIsHiddenByDefault(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "boss-bar-default-player");

    GameTestHelpers.assertTrue(
        helper, "A new NPC shows no boss bar", tickBossBar(easyNPC).getPlayers().isEmpty());
  }

  public static void assertBossBarIsShownToPlayerInRange(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "boss-bar-range-player");
    AttributeHandler.setDisplayAttribute(easyNPC, DisplayAttributeType.SHOW_BOSS_BAR, true);

    GameTestHelpers.assertTrue(
        helper,
        "A player in range sees the boss bar",
        tickBossBar(easyNPC).getPlayers().contains(serverPlayer));
  }

  public static void assertBossBarIsHiddenFromPlayerOutOfRange(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "boss-bar-distant-player");
    serverPlayer.setPos(
        serverPlayer.getX() + DisplayAttributeType.MAX_BOSS_BAR_RANGE + 10.0,
        serverPlayer.getY(),
        serverPlayer.getZ());
    AttributeHandler.setDisplayAttribute(easyNPC, DisplayAttributeType.SHOW_BOSS_BAR, true);

    GameTestHelpers.assertTrue(
        helper,
        "A player out of range does not see the boss bar",
        !tickBossBar(easyNPC).getPlayers().contains(serverPlayer));
  }

  public static void assertBossBarIsRemovedAfterDeath(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "boss-bar-death-player");
    AttributeHandler.setDisplayAttribute(easyNPC, DisplayAttributeType.SHOW_BOSS_BAR, true);
    BossBarHandler bossBarHandler = tickBossBar(easyNPC);

    easyNPC.getLivingEntity().kill();
    bossBarHandler.tick(easyNPC);

    GameTestHelpers.assertTrue(
        helper, "A dead NPC shows no boss bar", bossBarHandler.getPlayers().isEmpty());
  }

  public static void assertInvalidBossBarValuesAreRejected(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    GameTestHelpers.assertTrue(
        helper,
        "An unknown boss bar color is rejected",
        !AttributeHandler.setDisplayAttribute(
            easyNPC, DisplayAttributeType.BOSS_BAR_COLOR, "RAINBOW"));
    GameTestHelpers.assertTrue(
        helper,
        "A boss bar range above the limit is rejected",
        !AttributeHandler.setDisplayAttribute(
            easyNPC,
            DisplayAttributeType.BOSS_BAR_RANGE,
            DisplayAttributeType.MAX_BOSS_BAR_RANGE + 1));
    GameTestHelpers.assertEquals(
        helper,
        "The boss bar color keeps its default",
        BossEvent.BossBarColor.PURPLE,
        BossBarHandler.getBossBarColor(easyNPC.getEasyNPCDisplayAttributeData()));
  }

  public static void assertBossBarSettingsAreExportedWithThePreset(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setDisplayAttribute(easyNPC, DisplayAttributeType.SHOW_BOSS_BAR, true);
    AttributeHandler.setDisplayAttribute(
        easyNPC, DisplayAttributeType.BOSS_BAR_COLOR, BossEvent.BossBarColor.RED.name());
    AttributeHandler.setDisplayAttribute(
        easyNPC, DisplayAttributeType.BOSS_BAR_STYLE, BossEvent.BossBarOverlay.NOTCHED_10.name());
    AttributeHandler.setDisplayAttribute(
        easyNPC, DisplayAttributeType.BOSS_BAR_RANGE, PRESET_RANGE);

    CompoundTag presetTag = easyNPC.getEasyNPCPresetData().serializePresetData();
    EasyNPC<?> importedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SECOND_NPC_POSITION);
    importedNPC.getEasyNPCPresetData().importPresetData(presetTag);

    GameTestHelpers.assertTrue(
        helper,
        "The boss bar toggle is part of an exported preset",
        importedNPC
            .getEasyNPCDisplayAttributeData()
            .getDisplayBooleanAttribute(DisplayAttributeType.SHOW_BOSS_BAR));
    GameTestHelpers.assertEquals(
        helper,
        "The boss bar color is part of an exported preset",
        BossEvent.BossBarColor.RED,
        BossBarHandler.getBossBarColor(importedNPC.getEasyNPCDisplayAttributeData()));
    GameTestHelpers.assertEquals(
        helper,
        "The boss bar style is part of an exported preset",
        BossEvent.BossBarOverlay.NOTCHED_10,
        BossBarHandler.getBossBarStyle(importedNPC.getEasyNPCDisplayAttributeData()));
    GameTestHelpers.assertEquals(
        helper,
        "The boss bar range is part of an exported preset",
        PRESET_RANGE,
        BossBarHandler.getBossBarRange(importedNPC.getEasyNPCDisplayAttributeData()));
  }
}
