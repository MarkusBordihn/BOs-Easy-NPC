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
import de.markusbordihn.easynpc.data.type.ValueType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.VisibilityHandler;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class VisibilityAttributeTestHelper {

  private static final long DAY_TIME = 6000L;
  private static final long NIGHT_TIME = 18000L;

  private VisibilityAttributeTestHelper() {}

  public static void assertNightVisibilityIsRespected(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    ServerLevel level = helper.getLevel();
    long previousDayTime = getDayTime(level);

    try {
      setDayTime(level, NIGHT_TIME);
      GameTestHelpers.assertTrue(
          helper,
          "NPC must be visible at night as long as it is not disabled",
          !easyNPC.getEntity().isInvisibleTo(serverPlayer));

      setDisplayAttribute(easyNPC, DisplayAttributeType.VISIBLE_AT_NIGHT, false);
      GameTestHelpers.assertTrue(
          helper,
          "NPC must be invisible at night after disabling the night visibility",
          easyNPC.getEntity().isInvisibleTo(serverPlayer));

      setDayTime(level, DAY_TIME);
      GameTestHelpers.assertTrue(
          helper,
          "NPC must be visible at day after disabling only the night visibility",
          !easyNPC.getEntity().isInvisibleTo(serverPlayer));
    } finally {
      setDayTime(level, previousDayTime);
    }
  }

  public static void assertOwnerDoesNotOverrideNightVisibility(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    easyNPC.getEasyNPCOwnerData().setNPCOwner(serverPlayer);
    ServerLevel level = helper.getLevel();
    long previousDayTime = getDayTime(level);

    try {
      setDisplayAttribute(easyNPC, DisplayAttributeType.VISIBLE_AT_NIGHT, false);
      setDayTime(level, NIGHT_TIME);
      GameTestHelpers.assertTrue(
          helper,
          "Night visibility must also apply to the owner of the NPC",
          easyNPC.getEntity().isInvisibleTo(serverPlayer));
    } finally {
      setDayTime(level, previousDayTime);
    }
  }

  public static void assertOwnerVisibilityIsRespected(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    easyNPC.getEasyNPCOwnerData().setNPCOwner(serverPlayer);

    GameTestHelpers.assertTrue(
        helper,
        "NPC must be visible to its owner by default",
        !easyNPC.getEntity().isInvisibleTo(serverPlayer));

    setDisplayAttribute(easyNPC, DisplayAttributeType.VISIBLE_TO_OWNER, false);
    GameTestHelpers.assertTrue(
        helper,
        "NPC must be invisible to its owner after disabling the owner visibility",
        easyNPC.getEntity().isInvisibleTo(serverPlayer));
  }

  public static void assertGameModeVisibilityIsRespected(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));

    GameTestHelpers.assertTrue(
        helper,
        "Mocked test player must be in creative mode",
        serverPlayer.isCreative() && !serverPlayer.isSpectator());

    setDisplayAttribute(easyNPC, DisplayAttributeType.VISIBLE_IN_CREATIVE, false);
    GameTestHelpers.assertTrue(
        helper,
        "NPC must be invisible in creative mode after disabling the creative visibility",
        easyNPC.getEntity().isInvisibleTo(serverPlayer));

    setDisplayAttribute(easyNPC, DisplayAttributeType.VISIBLE_IN_CREATIVE, true);
    GameTestHelpers.assertTrue(
        helper,
        "NPC must be visible in creative mode after enabling the creative visibility",
        !easyNPC.getEntity().isInvisibleTo(serverPlayer));
  }

  public static void assertPreviewResolvesSimulatedDayTime(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));
    ServerLevel level = helper.getLevel();
    setDayTime(level, DAY_TIME);

    setDisplayAttribute(easyNPC, DisplayAttributeType.VISIBLE_AT_NIGHT, false);

    GameTestHelpers.assertTrue(
        helper,
        "Preview must report the NPC as visible for a simulated day time",
        VisibilityHandler.isVisibleToPlayerAtDayTime(easyNPC, serverPlayer, DAY_TIME));
    GameTestHelpers.assertTrue(
        helper,
        "Preview must report the NPC as hidden for a simulated night time",
        !VisibilityHandler.isVisibleToPlayerAtDayTime(easyNPC, serverPlayer, NIGHT_TIME));
    GameTestHelpers.assertTrue(
        helper, "Preview must not change the world time", getDayTime(level) == DAY_TIME);
  }

  public static void assertMainVisibilityIsRespected(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 2));

    setDisplayAttribute(easyNPC, DisplayAttributeType.VISIBLE, false);
    GameTestHelpers.assertTrue(
        helper,
        "NPC must be invisible after disabling the main visibility",
        easyNPC.getEntity().isInvisibleTo(serverPlayer));
  }

  private static long getDayTime(ServerLevel level) {
    return level.getDefaultClockTime();
  }

  private static void setDayTime(ServerLevel level, long dayTime) {
    level
        .dimensionType()
        .defaultClock()
        .ifPresent(clock -> level.clockManager().setTotalTicks(clock, dayTime));
  }

  private static void setDisplayAttribute(
      EasyNPC<?> easyNPC, DisplayAttributeType displayAttributeType, boolean value) {
    DisplayAttributeDataCapable<?> displayAttributeData = easyNPC.getEasyNPCDisplayAttributeData();
    displayAttributeData.setDisplayAttribute(displayAttributeType, ValueType.BOOLEAN, value);
  }
}
