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

import de.markusbordihn.easynpc.data.action.ActionDataEntry;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.action.ActionDataType;
import de.markusbordihn.easynpc.data.action.ActionEventSet;
import de.markusbordihn.easynpc.data.action.ActionEventType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.display.DisplayAttributeType;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.type.ValueType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.DisplayAttributeDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable;
import de.markusbordihn.easynpc.handler.PresetHandler;
import java.util.List;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.nbt.Tag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.phys.Vec3;

public class PresetRoundTripTestHelper {

  private static final Vec3 SOURCE_NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 TARGET_NPC_POSITION = new Vec3(2, 2, 2);
  private static final String NPC_NAME = "Round Trip";
  private static final String DIALOG_NAME = "Round Trip Dialog";
  private static final String DIALOG_TEXT = "The preset survived the round trip.";
  private static final int FOLLOW_OWNER_PRIORITY = 4;
  private static final int LIGHT_LEVEL = 12;

  private static final List<String> NON_COMPACT_TAGS =
      List.of(
          "Attributes",
          "Brain",
          "ArmorItems",
          "ArmorDropChances",
          "HandItems",
          "HandDropChances",
          "CanPickUpLoot",
          "LeftHanded",
          "PersistenceRequired",
          "Offers",
          "DisplayAttribute",
          "EntityAttribute",
          "TradingData",
          "SoundData",
          "Pos",
          "Rotation",
          "Owner",
          "Navigation");

  // Vanilla turns "VillagerDataFinalized" on whenever "VillagerData" is read, so an import always
  // sets it, no matter what the preset stores.
  private static final List<String> NON_COMPARABLE_TAGS =
      List.of(
          "UUID",
          "PresetUUID",
          "Pos",
          "Rotation",
          "Motion",
          "Navigation",
          "Brain",
          "StatusData",
          "PresetMetadata",
          "VillagerDataFinalized");

  private PresetRoundTripTestHelper() {}

  public static void assertPresetSurvivesRoundTrip(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> sourceNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SOURCE_NPC_POSITION);
    applyTestConfiguration(helper, sourceNPC);

    CompoundTag exportedTag = PresetHandler.prepareExportData(sourceNPC);

    EasyNPC<?> targetNPC = GameTestHelpers.mockEasyNPC(helper, entityType, TARGET_NPC_POSITION);
    targetNPC.registerEasyNPCDefaultData();
    ((PresetDataCapable<?>) targetNPC).importPresetData(exportedTag.copy());

    CompoundTag reExportedTag = PresetHandler.prepareExportData(targetNPC);

    String difference = findFirstDifference("", exportedTag, reExportedTag);
    GameTestHelpers.assertTrue(
        helper, "The preset must survive a round trip, but " + difference, difference == null);
  }

  public static void assertExportIsCompact(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SOURCE_NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();

    CompoundTag exportedTag = PresetHandler.prepareExportData(easyNPC);

    for (String unwantedTag : NON_COMPACT_TAGS) {
      GameTestHelpers.assertTrue(
          helper,
          "An unchanged NPC must not export " + unwantedTag,
          !exportedTag.contains(unwantedTag));
    }
  }

  public static void assertConfigurationSurvivesRoundTrip(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> sourceNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SOURCE_NPC_POSITION);
    applyTestConfiguration(helper, sourceNPC);

    CompoundTag exportedTag = ((PresetDataCapable<?>) sourceNPC).serializePresetData();

    EasyNPC<?> targetNPC = GameTestHelpers.mockEasyNPC(helper, entityType, TARGET_NPC_POSITION);
    ((PresetDataCapable<?>) targetNPC).importPresetData(exportedTag.copy());

    GameTestHelpers.assertNotNull(
        helper, "The imported NPC must keep its name", targetNPC.getEntity().getCustomName());
    GameTestHelpers.assertEquals(
        helper,
        "The imported NPC must keep its name",
        NPC_NAME,
        targetNPC.getEntity().getCustomName().getString());

    ObjectiveDataEntry importedObjective =
        targetNPC.getEasyNPCObjectiveData().getObjective(ObjectiveType.FOLLOW_OWNER);
    GameTestHelpers.assertNotNull(
        helper, "The imported NPC must keep its objective", importedObjective);
    GameTestHelpers.assertTrue(
        helper,
        "The imported NPC must not keep the standard objectives it replaced",
        targetNPC.getEasyNPCObjectiveData().getObjective(ObjectiveType.LOOK_AT_PLAYER) == null);

    GameTestHelpers.assertEquals(
        helper,
        "The imported NPC must keep its light level",
        LIGHT_LEVEL,
        ((DisplayAttributeDataCapable<?>) targetNPC)
            .getDisplayIntAttribute(DisplayAttributeType.LIGHT_LEVEL));

    DialogDataSet importedDialogDataSet = ((DialogDataCapable<?>) targetNPC).getDialogDataSet();
    GameTestHelpers.assertTrue(
        helper,
        "The imported NPC must keep its dialog",
        importedDialogDataSet != null && importedDialogDataSet.hasDialog());
  }

  private static void applyTestConfiguration(GameTestHelper helper, EasyNPC<?> easyNPC) {
    easyNPC.getEntity().setCustomName(Component.literal(NPC_NAME));

    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    GameTestHelpers.assertNotNull(helper, "NPC must support objectives", objectiveData);
    objectiveData.addOrUpdateCustomObjective(
        new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER, FOLLOW_OWNER_PRIORITY));

    ((DisplayAttributeDataCapable<?>) easyNPC)
        .setDisplayAttribute(DisplayAttributeType.LIGHT_LEVEL, ValueType.INTEGER, LIGHT_LEVEL);

    DialogDataSet dialogDataSet = new DialogDataSet();
    DialogDataEntry dialogDataEntry = new DialogDataEntry(DIALOG_NAME, DIALOG_TEXT);
    dialogDataSet.setDialog(dialogDataEntry.getId(), dialogDataEntry);
    ((DialogDataCapable<?>) easyNPC).setDialogDataSet(dialogDataSet);

    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(new ActionDataEntry(ActionDataType.OPEN_DEFAULT_DIALOG));
    ActionEventSet actionEventSet = new ActionEventSet();
    actionEventSet.setActionEvent(ActionEventType.ON_INTERACTION, actionDataSet);
    ((ActionEventDataCapable<?>) easyNPC).setActionEventSet(actionEventSet);
  }

  private static String findFirstDifference(
      String path, CompoundTag expectedTag, CompoundTag actualTag) {
    for (String key : expectedTag.keySet()) {
      if (path.isEmpty() && NON_COMPARABLE_TAGS.contains(key)) {
        continue;
      }

      String keyPath = path.isEmpty() ? key : path + "/" + key;
      if (!actualTag.contains(key)) {
        return keyPath + " is missing after the round trip";
      }

      Tag expectedValue = expectedTag.get(key);
      Tag actualValue = actualTag.get(key);
      if (expectedValue instanceof CompoundTag expectedCompoundTag
          && actualValue instanceof CompoundTag actualCompoundTag) {
        String difference = findFirstDifference(keyPath, expectedCompoundTag, actualCompoundTag);
        if (difference != null) {
          return difference;
        }
        continue;
      }

      if (!expectedValue.equals(actualValue)) {
        return keyPath + " changed from " + expectedValue + " to " + actualValue;
      }
    }

    for (String key : actualTag.keySet()) {
      if (path.isEmpty() && NON_COMPARABLE_TAGS.contains(key)) {
        continue;
      }

      if (!expectedTag.contains(key)) {
        return (path.isEmpty() ? key : path + "/" + key)
            + " was added by the round trip: "
            + actualTag.get(key);
      }
    }

    return null;
  }
}
