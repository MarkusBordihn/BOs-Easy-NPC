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
import de.markusbordihn.easynpc.handler.AttributeHandler;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.phys.Vec3;

public class AttributeCommandTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);

  private AttributeCommandTestHelper() {}

  private static EasyNPC<?> mockSpawnedNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();
    return easyNPC;
  }

  private static int executeAttributeCommand(
      GameTestHelper helper, EasyNPC<?> easyNPC, String attributeAndValue) {
    return GameTestHelpers.executeCommand(
        helper,
        GameTestHelpers.consoleCommandSource(helper, NPC_POSITION),
        "easy_npc attribute " + easyNPC.getEntityUUID() + " " + attributeAndValue);
  }

  public static void assertLightLevelCommandSetsLightLevel(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSpawnedNPC(helper, entityType);

    int result = executeAttributeCommand(helper, easyNPC, "light_level 15");

    GameTestHelpers.assertEquals(helper, "Light level command result", 1, result);
    GameTestHelpers.assertEquals(
        helper,
        "Light level",
        15,
        easyNPC
            .getEasyNPCDisplayAttributeData()
            .getDisplayIntAttribute(DisplayAttributeType.LIGHT_LEVEL));
  }

  public static void assertLightLevelOutOfRangeIsRejected(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSpawnedNPC(helper, entityType);
    executeAttributeCommand(helper, easyNPC, "light_level 7");

    int result = executeAttributeCommand(helper, easyNPC, "light_level 16");

    GameTestHelpers.assertEquals(helper, "Out of range light level command result", 0, result);
    GameTestHelpers.assertTrue(
        helper,
        "Light level handler must reject 20",
        !AttributeHandler.setDisplayAttribute(easyNPC, DisplayAttributeType.LIGHT_LEVEL, 20));
    GameTestHelpers.assertEquals(
        helper,
        "Light level after rejected values",
        7,
        easyNPC
            .getEasyNPCDisplayAttributeData()
            .getDisplayIntAttribute(DisplayAttributeType.LIGHT_LEVEL));
  }

  public static void assertMaxHealthCommandSetsMaxHealthAndHealth(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSpawnedNPC(helper, entityType);

    int result = executeAttributeCommand(helper, easyNPC, "max_health 40");

    LivingEntity livingEntity = easyNPC.getLivingEntity();
    GameTestHelpers.assertEquals(helper, "Max health command result", 1, result);
    GameTestHelpers.assertEquals(
        helper, "Max health", 40.0, livingEntity.getAttributeBaseValue(Attributes.MAX_HEALTH));
    GameTestHelpers.assertEquals(helper, "Health", 40.0F, livingEntity.getHealth());
  }

  public static void assertMaxHealthOutOfRangeIsRejected(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSpawnedNPC(helper, entityType);
    LivingEntity livingEntity = easyNPC.getLivingEntity();
    double maxHealth = livingEntity.getAttributeBaseValue(Attributes.MAX_HEALTH);

    int result = executeAttributeCommand(helper, easyNPC, "max_health 2000");

    GameTestHelpers.assertEquals(helper, "Out of range max health command result", 0, result);
    GameTestHelpers.assertEquals(
        helper,
        "Max health after rejected value",
        maxHealth,
        livingEntity.getAttributeBaseValue(Attributes.MAX_HEALTH));
  }
}
