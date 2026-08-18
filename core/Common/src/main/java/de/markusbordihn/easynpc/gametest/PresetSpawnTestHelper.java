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

import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.preset.PresetData;
import de.markusbordihn.easynpc.data.preset.PresetDataUtils;
import de.markusbordihn.easynpc.data.sound.SoundType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.DialogDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.SoundDataCapable;
import de.markusbordihn.easynpc.handler.PresetHandler;
import java.util.List;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.Vec3;

public class PresetSpawnTestHelper {

  private static final Vec3 SOURCE_NPC_POSITION = new Vec3(1, 2, 1);
  private static final BlockPos SPAWN_POSITION = new BlockPos(2, 2, 2);
  private static final String NPC_NAME = "Spawned From Preset";
  private static final String DIALOG_NAME = "Spawn Dialog";
  private static final String DIALOG_TEXT = "The spawned NPC kept its dialog.";
  private static final int FOLLOW_OWNER_PRIORITY = 4;
  private static final double SEARCH_RANGE = 3.0;
  private static final List<SoundType> EXPECTED_SOUND_TYPES =
      List.of(SoundType.DEATH, SoundType.HURT);

  private PresetSpawnTestHelper() {}

  public static void assertPresetItemSpawnsConfiguredNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    ItemStack presetItemStack =
        PresetDataUtils.toItemStack(createCompactPreset(helper, entityType));
    GameTestHelpers.assertTrue(
        helper, "A compact preset must fit into a preset item", !presetItemStack.isEmpty());

    PresetData presetData = PresetDataUtils.fromItemStack(presetItemStack);
    GameTestHelpers.assertTrue(
        helper, "A preset item must return a usable preset", presetData.hasValidData());

    assertSpawnKeepsConfigurationAndDefaults(helper, entityType, presetData, "preset item");
  }

  public static void assertSpawnerSpawnsConfiguredNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    PresetData presetData =
        PresetDataUtils.fromSpawnData(
            PresetDataUtils.toSpawnData(createCompactPreset(helper, entityType)));
    GameTestHelpers.assertTrue(
        helper, "A spawner must return a usable preset", presetData.hasValidData());

    assertSpawnKeepsConfigurationAndDefaults(helper, entityType, presetData, "spawner");
  }

  public static void assertPresetImportSetsHome(GameTestHelper helper, EntityType<?> entityType) {
    PresetData presetData = createCompactPreset(helper, entityType);
    BlockPos spawnPosition = helper.absolutePos(SPAWN_POSITION);
    Vec3 spawnLocation = Vec3.atBottomCenterOf(spawnPosition);

    GameTestHelpers.assertTrue(
        helper,
        "Importing a preset must spawn an NPC",
        PresetHandler.importPreset(helper.getLevel(), presetData, spawnLocation, null, null));

    Mob spawnedEntity = findSpawnedNPC(helper, entityType, spawnPosition);
    GameTestHelpers.assertNotNull(helper, "The imported preset must spawn its NPC", spawnedEntity);
    assertHomePosition(helper, (EasyNPC<?>) spawnedEntity, spawnPosition, "imported preset");
    spawnedEntity.discard();
  }

  private static PresetData createCompactPreset(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> sourceNPC = GameTestHelpers.mockEasyNPC(helper, entityType, SOURCE_NPC_POSITION);
    sourceNPC.registerEasyNPCDefaultData();
    sourceNPC.getEntity().setCustomName(Component.literal(NPC_NAME));
    sourceNPC
        .getEasyNPCObjectiveData()
        .addOrUpdateCustomObjective(
            new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER, FOLLOW_OWNER_PRIORITY));

    DialogDataSet dialogDataSet = new DialogDataSet();
    DialogDataEntry dialogDataEntry = new DialogDataEntry(DIALOG_NAME, DIALOG_TEXT);
    dialogDataSet.setDialog(dialogDataEntry.getId(), dialogDataEntry);
    ((DialogDataCapable<?>) sourceNPC).setDialogDataSet(dialogDataSet);

    CompoundTag exportedTag = PresetHandler.prepareExportData(sourceNPC);
    GameTestHelpers.assertNotNull(helper, "The source NPC must be exportable", exportedTag);
    sourceNPC.getEntity().discard();

    return new PresetData(entityType, exportedTag);
  }

  private static void assertSpawnKeepsConfigurationAndDefaults(
      GameTestHelper helper, EntityType<?> entityType, PresetData presetData, String spawnSource) {
    BlockPos spawnPosition = helper.absolutePos(SPAWN_POSITION);
    GameTestHelpers.assertTrue(
        helper,
        "A compact preset must spawn an NPC through the " + spawnSource,
        PresetDataUtils.spawnEntity(presetData, helper.getLevel(), spawnPosition));

    Mob spawnedEntity = findSpawnedNPC(helper, entityType, spawnPosition);
    GameTestHelpers.assertNotNull(
        helper, "The " + spawnSource + " must spawn the configured NPC", spawnedEntity);

    EasyNPC<?> spawnedNPC = (EasyNPC<?>) spawnedEntity;
    assertHomePosition(helper, spawnedNPC, spawnPosition, spawnSource);
    GameTestHelpers.assertNotNull(
        helper,
        "The NPC of the " + spawnSource + " must keep its objective",
        spawnedNPC.getEasyNPCObjectiveData().getObjective(ObjectiveType.FOLLOW_OWNER));
    DialogDataSet dialogDataSet = ((DialogDataCapable<?>) spawnedNPC).getDialogDataSet();
    GameTestHelpers.assertTrue(
        helper,
        "The NPC of the " + spawnSource + " must keep its dialog",
        dialogDataSet != null && dialogDataSet.hasDialog());

    SoundDataCapable<?> soundData = (SoundDataCapable<?>) spawnedNPC;
    for (SoundType soundType : EXPECTED_SOUND_TYPES) {
      GameTestHelpers.assertTrue(
          helper,
          "The NPC of the " + spawnSource + " must resolve its default sound " + soundType,
          soundData.hasDefaultSound(soundType));
    }

    GameTestHelpers.assertTrue(
        helper,
        "The NPC of the " + spawnSource + " must spawn alive",
        spawnedEntity.isAlive() && spawnedEntity.getHealth() == spawnedEntity.getMaxHealth());

    spawnedEntity.discard();
  }

  private static void assertHomePosition(
      GameTestHelper helper, EasyNPC<?> easyNPC, BlockPos expectedPosition, String spawnSource) {
    NavigationDataCapable<?> navigationData = easyNPC.getEasyNPCNavigationData();
    GameTestHelpers.assertTrue(
        helper,
        "The NPC of the " + spawnSource + " must use its spawn position as home",
        navigationData != null && expectedPosition.equals(navigationData.getHomePosition()));
  }

  private static Mob findSpawnedNPC(
      GameTestHelper helper, EntityType<?> entityType, BlockPos spawnPosition) {
    List<Mob> entities =
        helper
            .getLevel()
            .getEntitiesOfClass(
                Mob.class,
                new AABB(spawnPosition).inflate(SEARCH_RANGE),
                entity ->
                    entity instanceof EasyNPC<?>
                        && entity.getType() == entityType
                        && entity.getCustomName() != null
                        && NPC_NAME.equals(entity.getCustomName().getString()));

    return entities.size() == 1 ? entities.get(0) : null;
  }
}
