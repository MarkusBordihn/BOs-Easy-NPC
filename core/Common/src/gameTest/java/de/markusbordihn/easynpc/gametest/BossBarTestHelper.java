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
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.BossEvent;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.level.storage.TagValueInput;
import net.minecraft.world.level.storage.TagValueOutput;
import net.minecraft.world.phys.Vec3;

public class BossBarTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 SECOND_NPC_POSITION = new Vec3(1, 2, 2);
  private static final Vec3 PLAYER_POSITION = new Vec3(1, 2, 0);
  private static final int PRESET_RANGE = 48;
  private static final int PRESET_AUTO_HIDE_SECONDS = 15;

  private BossBarTestHelper() {}

  private static BossBarHandler tickBossBar(GameTestHelper helper, EasyNPC<?> easyNPC) {
    BossBarHandler bossBarHandler = new BossBarHandler();
    for (ServerPlayer serverPlayer : helper.getLevel().players()) {
      bossBarHandler.startSeenByPlayer(serverPlayer);
    }
    easyNPC.getLivingEntity().tickCount = 0;
    bossBarHandler.tick(easyNPC);
    return bossBarHandler;
  }

  private static int executeBossBarCommand(
      GameTestHelper helper, EasyNPC<?> easyNPC, String arguments) {
    return GameTestHelpers.executeCommand(
        helper,
        GameTestHelpers.consoleCommandSource(helper, NPC_POSITION),
        "easy_npc boss_bar " + easyNPC.getEntityUUID() + " " + arguments);
  }

  private static void tickBossBarAt(
      BossBarHandler bossBarHandler, EasyNPC<?> easyNPC, int tickCount) {
    easyNPC.getLivingEntity().tickCount = tickCount;
    bossBarHandler.tick(easyNPC);
  }

  public static void assertBossBarIsRemovedWhenPlayerStopsSeeingNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "boss-bar-unseen-player");
    AttributeHandler.setDisplayAttribute(easyNPC, DisplayAttributeType.SHOW_BOSS_BAR, true);
    BossBarHandler bossBarHandler = tickBossBar(helper, easyNPC);

    bossBarHandler.stopSeenByPlayer(serverPlayer);
    GameTestHelpers.assertTrue(
        helper,
        "The boss bar is removed when the player stops seeing the NPC",
        !bossBarHandler.getPlayers().contains(serverPlayer));

    tickBossBarAt(bossBarHandler, easyNPC, 10);
    GameTestHelpers.assertTrue(
        helper,
        "The boss bar is not shown again while the player does not see the NPC",
        !bossBarHandler.getPlayers().contains(serverPlayer));
  }

  public static void assertBossBarIsAutoHiddenAndShownAgainWhenFighting(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "boss-bar-auto-hide-player");
    AttributeHandler.setDisplayAttribute(easyNPC, DisplayAttributeType.SHOW_BOSS_BAR, true);
    AttributeHandler.setDisplayAttribute(easyNPC, DisplayAttributeType.BOSS_BAR_AUTO_HIDE, 1);
    BossBarHandler bossBarHandler = tickBossBar(helper, easyNPC);
    GameTestHelpers.assertTrue(
        helper,
        "The boss bar is shown before the auto hide time",
        bossBarHandler.getPlayers().contains(serverPlayer));

    tickBossBarAt(bossBarHandler, easyNPC, 20);
    GameTestHelpers.assertTrue(
        helper,
        "The boss bar is hidden after the auto hide time",
        !bossBarHandler.getPlayers().contains(serverPlayer));

    tickBossBarAt(bossBarHandler, easyNPC, 30);
    GameTestHelpers.assertTrue(
        helper,
        "The boss bar stays hidden while nothing happens",
        !bossBarHandler.getPlayers().contains(serverPlayer));

    easyNPC.getLivingEntity().tickCount = 40;
    easyNPC.getLivingEntity().setLastHurtByMob(serverPlayer);
    tickBossBarAt(bossBarHandler, easyNPC, 40);
    GameTestHelpers.assertTrue(
        helper,
        "The boss bar is shown again when the NPC is hurt",
        bossBarHandler.getPlayers().contains(serverPlayer));
  }

  public static void assertBossBarAutoHideIsResetWhenPlayerComesBack(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "boss-bar-returning-player");
    AttributeHandler.setDisplayAttribute(easyNPC, DisplayAttributeType.SHOW_BOSS_BAR, true);
    AttributeHandler.setDisplayAttribute(easyNPC, DisplayAttributeType.BOSS_BAR_AUTO_HIDE, 1);
    BossBarHandler bossBarHandler = tickBossBar(helper, easyNPC);
    tickBossBarAt(bossBarHandler, easyNPC, 20);
    GameTestHelpers.assertTrue(
        helper,
        "The boss bar is hidden after the auto hide time",
        !bossBarHandler.getPlayers().contains(serverPlayer));

    Vec3 playerPosition = serverPlayer.position();
    serverPlayer.setPos(
        playerPosition.x + DisplayAttributeType.MAX_BOSS_BAR_RANGE + 10.0,
        playerPosition.y,
        playerPosition.z);
    tickBossBarAt(bossBarHandler, easyNPC, 30);
    serverPlayer.setPos(playerPosition.x, playerPosition.y, playerPosition.z);
    tickBossBarAt(bossBarHandler, easyNPC, 40);
    GameTestHelpers.assertTrue(
        helper,
        "The boss bar is shown again when the player comes back into range",
        bossBarHandler.getPlayers().contains(serverPlayer));

    tickBossBarAt(bossBarHandler, easyNPC, 60);
    GameTestHelpers.assertTrue(
        helper,
        "The boss bar is hidden again after the auto hide time",
        !bossBarHandler.getPlayers().contains(serverPlayer));
  }

  public static void assertBossBarIsHiddenByDefault(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "boss-bar-default-player");

    GameTestHelpers.assertTrue(
        helper, "A new NPC shows no boss bar", tickBossBar(helper, easyNPC).getPlayers().isEmpty());
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
        tickBossBar(helper, easyNPC).getPlayers().contains(serverPlayer));
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
        !tickBossBar(helper, easyNPC).getPlayers().contains(serverPlayer));
  }

  public static void assertBossBarIsRemovedAfterDeath(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    GameTestHelpers.mockServerPlayer(helper, PLAYER_POSITION, "boss-bar-death-player");
    AttributeHandler.setDisplayAttribute(easyNPC, DisplayAttributeType.SHOW_BOSS_BAR, true);
    BossBarHandler bossBarHandler = tickBossBar(helper, easyNPC);

    easyNPC.getLivingEntity().kill(helper.getLevel());
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

  public static void assertBossBarCommandChangesSettings(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    GameTestHelpers.assertEquals(
        helper, "show succeeds", 1, executeBossBarCommand(helper, easyNPC, "show"));
    GameTestHelpers.assertEquals(
        helper, "color succeeds", 1, executeBossBarCommand(helper, easyNPC, "color red"));
    GameTestHelpers.assertEquals(
        helper, "style succeeds", 1, executeBossBarCommand(helper, easyNPC, "style notched_10"));
    GameTestHelpers.assertEquals(
        helper, "range succeeds", 1, executeBossBarCommand(helper, easyNPC, "range 64"));
    GameTestHelpers.assertEquals(
        helper, "auto_hide succeeds", 1, executeBossBarCommand(helper, easyNPC, "auto_hide 30"));
    GameTestHelpers.assertEquals(
        helper, "info succeeds", 1, executeBossBarCommand(helper, easyNPC, "info"));

    GameTestHelpers.assertTrue(
        helper,
        "show enables the boss bar",
        easyNPC
            .getEasyNPCDisplayAttributeData()
            .getDisplayBooleanAttribute(DisplayAttributeType.SHOW_BOSS_BAR));
    GameTestHelpers.assertEquals(
        helper,
        "color sets the boss bar color",
        BossEvent.BossBarColor.RED,
        BossBarHandler.getBossBarColor(easyNPC.getEasyNPCDisplayAttributeData()));
    GameTestHelpers.assertEquals(
        helper,
        "style sets the boss bar style",
        BossEvent.BossBarOverlay.NOTCHED_10,
        BossBarHandler.getBossBarStyle(easyNPC.getEasyNPCDisplayAttributeData()));
    GameTestHelpers.assertEquals(
        helper,
        "range sets the boss bar range",
        64,
        BossBarHandler.getBossBarRange(easyNPC.getEasyNPCDisplayAttributeData()));
    GameTestHelpers.assertEquals(
        helper,
        "auto_hide sets the auto hide time",
        30,
        BossBarHandler.getBossBarAutoHideSeconds(easyNPC.getEasyNPCDisplayAttributeData()));

    GameTestHelpers.assertEquals(
        helper, "hide succeeds", 1, executeBossBarCommand(helper, easyNPC, "hide"));
    GameTestHelpers.assertTrue(
        helper,
        "hide disables the boss bar",
        !easyNPC
            .getEasyNPCDisplayAttributeData()
            .getDisplayBooleanAttribute(DisplayAttributeType.SHOW_BOSS_BAR));
  }

  public static void assertBossBarCommandRejectsOutOfRangeValues(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    int initialRange = BossBarHandler.getBossBarRange(easyNPC.getEasyNPCDisplayAttributeData());

    GameTestHelpers.assertEquals(
        helper,
        "A range above the limit is rejected",
        0,
        executeBossBarCommand(
            helper, easyNPC, "range " + (DisplayAttributeType.MAX_BOSS_BAR_RANGE + 1)));
    GameTestHelpers.assertEquals(
        helper,
        "An auto hide time above the limit is rejected",
        0,
        executeBossBarCommand(
            helper,
            easyNPC,
            "auto_hide " + (DisplayAttributeType.MAX_BOSS_BAR_AUTO_HIDE_SECONDS + 1)));
    GameTestHelpers.assertEquals(
        helper,
        "The boss bar range keeps its value",
        initialRange,
        BossBarHandler.getBossBarRange(easyNPC.getEasyNPCDisplayAttributeData()));
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
    AttributeHandler.setDisplayAttribute(
        easyNPC, DisplayAttributeType.BOSS_BAR_AUTO_HIDE, PRESET_AUTO_HIDE_SECONDS);

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
    GameTestHelpers.assertEquals(
        helper,
        "The boss bar auto hide time is part of an exported preset",
        PRESET_AUTO_HIDE_SECONDS,
        BossBarHandler.getBossBarAutoHideSeconds(importedNPC.getEasyNPCDisplayAttributeData()));
  }

  public static void assertBossBarSettingsAreSavedAndLoaded(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setDisplayAttribute(easyNPC, DisplayAttributeType.SHOW_BOSS_BAR, true);
    AttributeHandler.setDisplayAttribute(
        easyNPC, DisplayAttributeType.BOSS_BAR_COLOR, BossEvent.BossBarColor.GREEN.name());
    AttributeHandler.setDisplayAttribute(
        easyNPC, DisplayAttributeType.BOSS_BAR_STYLE, BossEvent.BossBarOverlay.NOTCHED_6.name());
    AttributeHandler.setDisplayAttribute(
        easyNPC, DisplayAttributeType.BOSS_BAR_RANGE, PRESET_RANGE);
    AttributeHandler.setDisplayAttribute(
        easyNPC, DisplayAttributeType.BOSS_BAR_AUTO_HIDE, PRESET_AUTO_HIDE_SECONDS);

    TagValueOutput valueOutput =
        TagValueOutput.createWithContext(
            ProblemReporter.DISCARDING, helper.getLevel().registryAccess());
    easyNPC.getEntity().saveWithoutId(valueOutput);
    EasyNPC<?> loadedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SECOND_NPC_POSITION);
    loadedNPC
        .getEasyNPCDisplayAttributeData()
        .readAdditionalDisplayAttributeData(
            TagValueInput.create(
                ProblemReporter.DISCARDING,
                helper.getLevel().registryAccess(),
                valueOutput.buildResult()));

    GameTestHelpers.assertTrue(
        helper,
        "The boss bar toggle is saved and loaded",
        loadedNPC
            .getEasyNPCDisplayAttributeData()
            .getDisplayBooleanAttribute(DisplayAttributeType.SHOW_BOSS_BAR));
    GameTestHelpers.assertEquals(
        helper,
        "The boss bar color is saved and loaded",
        BossEvent.BossBarColor.GREEN,
        BossBarHandler.getBossBarColor(loadedNPC.getEasyNPCDisplayAttributeData()));
    GameTestHelpers.assertEquals(
        helper,
        "The boss bar style is saved and loaded",
        BossEvent.BossBarOverlay.NOTCHED_6,
        BossBarHandler.getBossBarStyle(loadedNPC.getEasyNPCDisplayAttributeData()));
    GameTestHelpers.assertEquals(
        helper,
        "The boss bar range is saved and loaded",
        PRESET_RANGE,
        BossBarHandler.getBossBarRange(loadedNPC.getEasyNPCDisplayAttributeData()));
    GameTestHelpers.assertEquals(
        helper,
        "The boss bar auto hide time is saved and loaded",
        PRESET_AUTO_HIDE_SECONDS,
        BossBarHandler.getBossBarAutoHideSeconds(loadedNPC.getEasyNPCDisplayAttributeData()));
  }
}
