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
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.handler.AttributeHandler;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.level.storage.TagValueInput;
import net.minecraft.world.level.storage.TagValueOutput;
import net.minecraft.world.phys.Vec3;

public class OpacityAttributeTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 SECOND_NPC_POSITION = new Vec3(1, 2, 2);
  private static final int GHOST_OPACITY = 40;

  private OpacityAttributeTestHelper() {}

  private static void setOpacityByAction(EasyNPC<?> easyNPC, String opacity) {
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(new ActionDataEntry(ActionDataType.SET_OPACITY, opacity));

    easyNPC
        .getEasyNPCActionHandler()
        .executeActions(actionDataSet, ActionContext.of(ActionEventType.ON_SPAWN, null, null));
  }

  public static void assertNewNPCIsFullyOpaque(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    GameTestHelpers.assertEquals(
        helper,
        "A new NPC is fully opaque",
        DisplayAttributeType.DEFAULT_OPACITY,
        AttributeHandler.getOpacity(easyNPC));
  }

  public static void assertOpacityActionChangesTheAttribute(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    setOpacityByAction(easyNPC, String.valueOf(GHOST_OPACITY));

    GameTestHelpers.assertEquals(
        helper,
        "The opacity action changes the attribute",
        GHOST_OPACITY,
        AttributeHandler.getOpacity(easyNPC));
  }

  public static void assertFullyTransparentOpacitySurvivesSaveAndLoad(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setOpacity(easyNPC, DisplayAttributeType.MIN_OPACITY);

    TagValueOutput valueOutput =
        TagValueOutput.createWithContext(
            ProblemReporter.DISCARDING, helper.getLevel().registryAccess());
    easyNPC.getEasyNPCDisplayAttributeData().addAdditionalDisplayAttributeData(valueOutput);

    EasyNPC<?> reloadedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SECOND_NPC_POSITION);
    reloadedNPC
        .getEasyNPCDisplayAttributeData()
        .readAdditionalDisplayAttributeData(
            TagValueInput.create(
                ProblemReporter.DISCARDING,
                helper.getLevel().registryAccess(),
                valueOutput.buildResult()));

    GameTestHelpers.assertEquals(
        helper,
        "A fully transparent NPC stays transparent after a reload",
        DisplayAttributeType.MIN_OPACITY,
        AttributeHandler.getOpacity(reloadedNPC));
  }

  public static void assertOpacityIsExportedWithThePreset(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    AttributeHandler.setOpacity(easyNPC, GHOST_OPACITY);

    CompoundTag presetTag = easyNPC.getEasyNPCPresetData().serializePresetData();
    EasyNPC<?> importedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SECOND_NPC_POSITION);
    importedNPC.getEasyNPCPresetData().importPresetData(presetTag);

    GameTestHelpers.assertEquals(
        helper,
        "The opacity is part of an exported preset",
        GHOST_OPACITY,
        AttributeHandler.getOpacity(importedNPC));
  }

  public static void assertOpacityIsClampedToTheAllowedRange(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    AttributeHandler.setOpacity(easyNPC, DisplayAttributeType.MAX_OPACITY + 50);

    GameTestHelpers.assertEquals(
        helper,
        "An opacity above the limit is clamped",
        DisplayAttributeType.MAX_OPACITY,
        AttributeHandler.getOpacity(easyNPC));

    AttributeHandler.setOpacity(easyNPC, DisplayAttributeType.MIN_OPACITY - 50);

    GameTestHelpers.assertEquals(
        helper,
        "An opacity below the limit is clamped",
        DisplayAttributeType.MIN_OPACITY,
        AttributeHandler.getOpacity(easyNPC));
  }
}
