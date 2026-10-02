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

package de.markusbordihn.easynpc.configui.gametest;

import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.SkinDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.StatusDataCapable;
import de.markusbordihn.easynpc.gametest.GameTestHelpers;
import java.util.List;
import java.util.function.Predicate;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.syncher.SynchedEntityData;
import net.minecraft.world.entity.Entity;

final class NPCPersistenceAssertions {

  private static final List<String> CREATION_DEPENDENT_TAGS =
      List.of(Entity.UUID_TAG, "Rotation", StatusDataCapable.DATA_STATUS_DATA_TAG);

  private NPCPersistenceAssertions() {}

  static void assertSurvivesRetrackingAndRespawn(
      GameTestHelper helper, EasyNPC<?> easyNPC, Predicate<EasyNPC<?>> isApplied) {
    CompoundTag retrackedBeforeRespawn = synchedDataTag(retrack(helper, easyNPC));
    EasyNPC<?> respawnedNPC = respawn(helper, easyNPC);
    GameTestHelpers.assertTrue(
        helper, "Change was lost after respawn", isApplied.test(respawnedNPC));
    GameTestHelpers.assertEquals(
        helper,
        "Newly tracking players see other synched data than after a reload",
        synchedDataTag(retrack(helper, respawnedNPC)),
        retrackedBeforeRespawn);
  }

  static EasyNPC<?> retrack(GameTestHelper helper, EasyNPC<?> easyNPC) {
    List<SynchedEntityData.DataValue<?>> pairingData =
        easyNPC.getEntity().getEntityData().getNonDefaultValues();
    Entity trackingEntity = easyNPC.getEntity().getType().create(helper.getLevel());
    GameTestHelpers.assertNotNull(helper, "Re-tracked entity is null", trackingEntity);
    if (pairingData != null) {
      trackingEntity.getEntityData().assignValues(pairingData);
    }
    return (EasyNPC<?>) trackingEntity;
  }

  static EasyNPC<?> respawn(GameTestHelper helper, EasyNPC<?> easyNPC) {
    CompoundTag savedNPC = easyNPC.getEntity().saveWithoutId(new CompoundTag());
    Entity respawnedEntity = easyNPC.getEntity().getType().create(helper.getLevel());
    easyNPC.getEntity().discard();
    GameTestHelpers.assertNotNull(helper, "Respawned entity is null", respawnedEntity);
    respawnedEntity.load(savedNPC);
    GameTestHelpers.assertTrue(
        helper,
        "Failed to respawn " + easyNPC.getEntity().getType(),
        helper.getLevel().addFreshEntity(respawnedEntity));
    return (EasyNPC<?>) respawnedEntity;
  }

  private static CompoundTag synchedDataTag(EasyNPC<?> trackingNPC) {
    CompoundTag synchedDataTag = trackingNPC.getEntity().saveWithoutId(new CompoundTag());
    for (String creationDependentTag : CREATION_DEPENDENT_TAGS) {
      synchedDataTag.remove(creationDependentTag);
    }
    synchedDataTag
        .getCompound(SkinDataCapable.EASY_NPC_DATA_SKIN_DATA_TAG)
        .remove(SkinDataEntry.DATA_TIMESTAMP_TAG);
    return synchedDataTag;
  }
}
