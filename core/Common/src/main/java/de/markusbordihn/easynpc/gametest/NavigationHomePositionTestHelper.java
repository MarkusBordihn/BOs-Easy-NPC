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

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.NavigationDataCapable;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.level.storage.TagValueInput;
import net.minecraft.world.phys.Vec3;

public final class NavigationHomePositionTestHelper {

  private NavigationHomePositionTestHelper() {}

  public static void assertSpawnedNPCHasHomePosition(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    if (easyNPC == null) {
      return;
    }

    NavigationDataCapable<?> navigationData = easyNPC.getEasyNPCNavigationData();
    if (navigationData == null) {
      helper.fail("Spawned NPC " + entityType + " has no navigation data");
      return;
    }

    if (!navigationData.hasNPCHomePosition()) {
      helper.fail(
          "Spawned NPC "
              + entityType
              + " at "
              + easyNPC.getEntity().blockPosition()
              + " has no home position, got "
              + navigationData.getNPCHomePosition());
    }
  }

  // Mob#getHomePosition() would shadow the interface default and always report BlockPos.ZERO.
  public static void assertHomePositionIsSynched(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    if (easyNPC == null) {
      return;
    }

    NavigationDataCapable<?> navigationData = easyNPC.getEasyNPCNavigationData();
    BlockPos homePosition = helper.absolutePos(new BlockPos(2, 2, 2));
    navigationData.setNPCHomePosition(homePosition);

    if (!homePosition.equals(navigationData.getNPCHomePosition())) {
      helper.fail(
          "Home position was not stored in the synched entity data, expected "
              + homePosition
              + " but got "
              + navigationData.getNPCHomePosition());
      return;
    }

    if (!navigationData.hasNPCHomePosition()) {
      helper.fail("Home position " + homePosition + " is not reported as set");
    }
  }

  public static void assertHomePositionSurvivesSaveAndLoad(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    if (easyNPC == null) {
      return;
    }

    BlockPos homePosition = helper.absolutePos(new BlockPos(2, 2, 2));
    easyNPC.getEasyNPCNavigationData().setNPCHomePosition(homePosition);

    ServerLevel serverLevel = helper.getLevel();
    CompoundTag savedData = easyNPC.getEasyNPCPresetData().serializePresetData();
    if (!savedData.contains(NavigationDataCapable.DATA_NAVIGATION_TAG)) {
      helper.fail("Saved NPC data has no " + NavigationDataCapable.DATA_NAVIGATION_TAG + " tag");
      return;
    }

    Entity reloadedEntity = entityType.create(serverLevel, EntitySpawnReason.COMMAND);
    reloadedEntity.load(
        TagValueInput.create(ProblemReporter.DISCARDING, serverLevel.registryAccess(), savedData));

    if (!(reloadedEntity instanceof EasyNPC<?> reloadedEasyNPC)) {
      helper.fail("Reloaded entity is not an EasyNPC: " + reloadedEntity);
      return;
    }

    if (!homePosition.equals(reloadedEasyNPC.getEasyNPCNavigationData().getNPCHomePosition())) {
      helper.fail(
          "Home position was lost after save and load, expected "
              + homePosition
              + " but got "
              + reloadedEasyNPC.getEasyNPCNavigationData().getNPCHomePosition());
    }
  }
}
