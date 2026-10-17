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

import de.markusbordihn.easynpc.api.handler.EasyNPCEntityHandler;
import de.markusbordihn.easynpc.data.action.ActionGroup;
import de.markusbordihn.easynpc.data.action.ActionManager;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.SavedNPCEntityEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.data.saveddata.NPCEntityData;
import de.markusbordihn.easynpc.data.status.StatusDataType;
import de.markusbordihn.easynpc.entity.EasyNPCEntityTypes;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ObjectiveDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.data.OwnerDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.npc.standard.HumanoidNPC;
import de.markusbordihn.easynpc.handler.OwnerHandler;
import de.markusbordihn.easynpc.handler.RespawnHandler;
import java.util.Collection;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.server.level.ServerLevel;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.MobSpawnType;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.phys.Vec3;

public class NPCEntityLifecycleTestHelper {

  private static final double POSITION_TOLERANCE = 1.0E-6D;

  private NPCEntityLifecycleTestHelper() {}

  private static boolean containsNPC(Collection<SavedNPCEntityEntry> entries, UUID entityUUID) {
    return entries.stream().anyMatch(entry -> entityUUID.equals(entry.entityUUID()));
  }

  private static NPCRemovalReason getStoredRemovalReason(GameTestHelper helper, UUID entityUUID) {
    Optional<SavedNPCEntityEntry> entry = NPCEntityManager.getNPC(entityUUID);
    if (entry.isEmpty() || entry.get().metadata() == null) {
      helper.fail("NPC " + entityUUID + " is missing from the NPC index");
      return null;
    }

    return entry.get().metadata().removalReason();
  }

  @SuppressWarnings("unchecked")
  private static HumanoidNPC spawnNonPersistentNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    HumanoidNPC easyNPC =
        new HumanoidNPC((EntityType<? extends PathfinderMob>) entityType, helper.getLevel()) {
          @Override
          public boolean isPersistentNPC() {
            return false;
          }
        };
    easyNPC.setPos(helper.absoluteVec(new Vec3(1, 2, 1)));
    GameTestHelpers.removeNPCDataAfterTest(helper, easyNPC.getEntityUUID());
    if (!helper.getLevel().addFreshEntity(easyNPC)) {
      helper.fail("Unable to add the non-persistent NPC to the level");
    }
    return easyNPC;
  }

  // A loader without the server started event makes every NPC query silently return nothing.
  public static void assertNPCEntityDataInitialized(GameTestHelper helper) {
    try {
      NPCEntityData.get();
    } catch (IllegalStateException e) {
      helper.fail("NPCEntityData is not initialized after server start: " + e.getMessage());
      return;
    }

    if (NPCEntityManager.getAllNPCs() == null) {
      helper.fail("NPCEntityManager is not usable after server start");
    }
  }

  public static void assertNPCIsIndexedOnSpawn(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(easyNPC);

    if (NPCEntityManager.getNPC(easyNPC.getEntityUUID()).isEmpty()) {
      helper.fail("Spawned NPC " + easyNPC.getEntityUUID() + " is missing from the NPC index");
    }
  }

  public static void assertRepeatedOwnerUpdateIsIdempotent(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(2, 2, 1), "owner-repeat-player");
    NPCEntityManager.saveNPC(easyNPC);

    OwnerHandler.setOwner(easyNPC, serverPlayer);
    if (!OwnerHandler.setOwner(easyNPC, serverPlayer)) {
      helper.fail("Setting the same owner again should succeed");
      return;
    }

    OwnerDataCapable<?> ownerData = easyNPC.getEasyNPCOwnerData();
    if (ownerData == null || !serverPlayer.getUUID().equals(ownerData.getOwnerUUID())) {
      helper.fail("Owner must stay unchanged after setting the same owner again");
    }
  }

  public static void assertFollowOwnerWithoutTargetIsRegistered(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(2, 2, 1), "follow-owner-player");
    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    if (objectiveData == null) {
      helper.fail("NPC has no objective data");
      return;
    }

    ObjectiveDataEntry followOwner = new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER);
    objectiveData.addOrUpdateCustomObjective(followOwner);
    if (followOwner.isRegistered()) {
      helper.fail("Follow owner objective must stay unregistered while the NPC has no owner");
    }

    OwnerHandler.setOwner(easyNPC, serverPlayer);

    ObjectiveDataEntry registeredObjective = objectiveData.getObjective(ObjectiveType.FOLLOW_OWNER);
    if (registeredObjective == null || !registeredObjective.isRegistered()) {
      helper.fail("Follow owner objective was not registered after the owner was set");
      return;
    }

    if (registeredObjective.getTargetOwnerUUID() != null) {
      helper.fail("Follow owner objective must stay owner agnostic, got a fixed target owner");
    }
  }

  public static void assertCustomIdentifierIsQueryable(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    ResourceLocation customIdentifier = new ResourceLocation("othermod", "companion");
    if (easyNPC.getEasyNPCPresetData() == null) {
      helper.fail("NPC has no preset data");
      return;
    }

    if (!EasyNPC.DEFAULT_CUSTOM_NPC_IDENTIFIER.equals(easyNPC.getCustomNPCIdentifier())) {
      helper.fail("NPC without a custom identifier should report the default one");
    }

    easyNPC.getEasyNPCPresetData().setCustomIdentifier(customIdentifier);
    NPCEntityManager.saveNPC(easyNPC);

    if (!customIdentifier.equals(easyNPC.getCustomNPCIdentifier())) {
      helper.fail(
          "NPC did not report its custom identifier, got " + easyNPC.getCustomNPCIdentifier());
    }
    if (!containsNPC(
        EasyNPCEntityHandler.getByCustomIdentifier(customIdentifier), easyNPC.getEntityUUID())) {
      helper.fail("NPC is missing from the custom identifier index");
    }
    if (!containsNPC(
        EasyNPCEntityHandler.getByCustomIdentifierNamespace("othermod"), easyNPC.getEntityUUID())) {
      helper.fail("NPC is missing from the custom identifier namespace index");
    }
  }

  public static void assertRespawnKeepsOwner(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(2, 2, 1), "respawn-owner-player");
    NPCEntityManager.saveNPC(easyNPC);
    OwnerHandler.setOwner(easyNPC, serverPlayer);
    NPCEntityManager.saveNPC(easyNPC);

    UUID entityUUID = easyNPC.getEntityUUID();
    if (!EasyNPCEntityHandler.despawn(easyNPC, NPCRemovalReason.DESPAWNED)) {
      helper.fail("Failed to despawn the NPC");
      return;
    }

    if (!EasyNPCEntityHandler.spawn(entityUUID, helper.getLevel())) {
      helper.fail("Failed to respawn the NPC");
      return;
    }

    EasyNPC<?> respawnedNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
    if (respawnedNPC == null) {
      helper.fail("Respawned NPC " + entityUUID + " was not found in the world");
      return;
    }

    OwnerDataCapable<?> ownerData = respawnedNPC.getEasyNPCOwnerData();
    if (ownerData == null || !serverPlayer.getUUID().equals(ownerData.getOwnerUUID())) {
      helper.fail(
          "Respawned NPC lost its owner, expected "
              + serverPlayer.getUUID()
              + ", got "
              + (ownerData != null ? ownerData.getOwnerUUID() : null));
    }
  }

  public static void assertRemovalReasonIsResetOnRespawn(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(easyNPC);

    UUID entityUUID = easyNPC.getEntityUUID();
    if (!EasyNPCEntityHandler.despawn(easyNPC, NPCRemovalReason.DESPAWNED)) {
      helper.fail("Failed to despawn the NPC");
      return;
    }

    if (getStoredRemovalReason(helper, entityUUID) != NPCRemovalReason.DESPAWNED) {
      helper.fail("Despawned NPC should be stored with the despawn removal reason");
      return;
    }

    if (!EasyNPCEntityHandler.spawn(entityUUID, helper.getLevel())) {
      helper.fail("Failed to respawn the NPC");
      return;
    }

    NPCRemovalReason removalReason = getStoredRemovalReason(helper, entityUUID);
    if (removalReason != NPCRemovalReason.NONE) {
      helper.fail("Respawned NPC still uses the removal reason " + removalReason);
    }
  }

  public static void assertDeletedNPCIsNotRestorableForOwner(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(2, 2, 1), "delete-restore-player");
    EasyNPC<?> despawnedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    EasyNPC<?> deletedNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(despawnedNPC);
    NPCEntityManager.saveNPC(deletedNPC);
    OwnerHandler.setOwner(despawnedNPC, serverPlayer);
    OwnerHandler.setOwner(deletedNPC, serverPlayer);

    UUID despawnedUUID = despawnedNPC.getEntityUUID();
    UUID deletedUUID = deletedNPC.getEntityUUID();
    if (!EasyNPCEntityHandler.despawn(despawnedNPC, NPCRemovalReason.UNLOADED_BY_PLAYER)) {
      helper.fail("Failed to despawn the NPC");
      return;
    }

    if (!EasyNPCEntityHandler.delete(deletedNPC)) {
      helper.fail("Failed to delete the NPC");
      return;
    }

    Collection<SavedNPCEntityEntry> ownedNPCs =
        NPCEntityManager.getNPCsByOwner(serverPlayer.getUUID());
    if (!containsNPC(ownedNPCs, despawnedUUID)) {
      helper.fail("Despawned NPC " + despawnedUUID + " must stay restorable for its owner");
      return;
    }

    if (containsNPC(ownedNPCs, deletedUUID) || NPCEntityManager.getNPC(deletedUUID).isPresent()) {
      helper.fail("Deleted NPC " + deletedUUID + " is still stored and returns on the owner login");
    }
  }

  public static void assertRespawnKeepsIndexEntry(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(easyNPC);

    UUID entityUUID = easyNPC.getEntityUUID();
    if (!RespawnHandler.respawnNPC(easyNPC, helper.getLevel())) {
      helper.fail("Failed to respawn the NPC");
      return;
    }

    if (LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel()) == null) {
      helper.fail("Respawned NPC " + entityUUID + " was not found in the world");
      return;
    }

    NPCRemovalReason removalReason = getStoredRemovalReason(helper, entityUUID);
    if (removalReason != NPCRemovalReason.NONE) {
      helper.fail("Respawn must not mark the NPC as removed, got " + removalReason);
    }
  }

  public static void assertLoadedNPCStaysPersistent(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    CompoundTag entityTag = easyNPC.getEntity().saveWithoutId(new CompoundTag());
    entityTag.putBoolean("PersistenceRequired", false);
    easyNPC.getEntity().load(entityTag);

    if (!easyNPC.getMob().isPersistenceRequired() || !easyNPC.isPersistentNPC()) {
      helper.fail("NPC loaded with PersistenceRequired:0b must stay persistent");
    }
  }

  public static void assertNonPersistentNPCIsNotStoredOnJoinAndUnload(
      GameTestHelper helper, EntityType<?> entityType) {
    HumanoidNPC easyNPC = spawnNonPersistentNPC(helper, entityType);
    UUID entityUUID = easyNPC.getEntityUUID();
    if (NPCEntityManager.getNPC(entityUUID).isPresent()) {
      helper.fail("Non-persistent NPC " + entityUUID + " must not be indexed on join");
      return;
    }

    easyNPC.discard();
    if (NPCEntityManager.getNPC(entityUUID).isPresent()) {
      helper.fail("Non-persistent NPC " + entityUUID + " must not be stored on unload");
    }
  }

  public static void assertNonPersistentNPCIsStoredOnExplicitDespawn(
      GameTestHelper helper, EntityType<?> entityType) {
    HumanoidNPC easyNPC = spawnNonPersistentNPC(helper, entityType);
    UUID entityUUID = easyNPC.getEntityUUID();
    if (!EasyNPCEntityHandler.despawn(easyNPC, NPCRemovalReason.DESPAWNED)) {
      helper.fail("Failed to despawn the non-persistent NPC");
      return;
    }

    if (getStoredRemovalReason(helper, entityUUID) != NPCRemovalReason.DESPAWNED) {
      helper.fail("Explicitly despawned non-persistent NPC must be stored as despawned");
    }
  }

  public static void assertEnsureInitializedKeepsNPCIndex(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(easyNPC);
    NPCEntityData entityData = NPCEntityData.get();

    NPCEntityManager.ensureInitialized(helper.getLevel().getServer());

    if (NPCEntityData.get() != entityData) {
      helper.fail("Repeated initialization for the same server replaced the NPC index");
      return;
    }

    if (NPCEntityManager.getNPC(easyNPC.getEntityUUID()).isEmpty()) {
      helper.fail("Repeated initialization lost the NPC " + easyNPC.getEntityUUID());
    }
  }

  public static void assertDeletedNPCIsDiscardedOnLoad(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerLevel serverLevel = helper.getLevel();
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(easyNPC);
    UUID entityUUID = easyNPC.getEntityUUID();
    CompoundTag entityTag = easyNPC.getEntity().saveWithoutId(new CompoundTag());
    GameTestHelpers.addCleanup(helper, () -> NPCEntityManager.clearNPCDeleted(entityUUID));

    if (!EasyNPCEntityHandler.delete(easyNPC)) {
      helper.fail("Failed to delete the NPC");
      return;
    }

    Entity reloadedEntity = entityType.create(serverLevel);
    if (reloadedEntity == null) {
      helper.fail("Unable to create " + entityType + " for the reload");
      return;
    }

    reloadedEntity.load(entityTag);
    if (!serverLevel.addFreshEntity(reloadedEntity)) {
      helper.fail("Unable to add the reloaded NPC " + entityUUID + " to the level");
      return;
    }

    saveDirtyNPCsBeforeScheduledDiscard(reloadedEntity);

    helper.succeedWhen(
        () -> {
          if (!reloadedEntity.isRemoved()) {
            helper.fail("Deleted NPC " + entityUUID + " returned on load");
          }
          if (NPCEntityManager.getNPC(entityUUID).isPresent()) {
            helper.fail("Deleted NPC " + entityUUID + " was indexed again on load");
          }
        });
  }

  private static void saveDirtyNPCsBeforeScheduledDiscard(Entity entity) {
    if (entity instanceof EasyNPC<?> easyNPC) {
      easyNPC.getEasyNPCStatusData().markNPCDataUpdated();
    }
    NPCEntityManager.saveAllDirtyNPCs();
  }

  public static void assertKilledNPCRequiresForcedSpawn(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(easyNPC);
    UUID entityUUID = easyNPC.getEntityUUID();
    if (!EasyNPCEntityHandler.despawn(easyNPC, NPCRemovalReason.KILLED)) {
      helper.fail("Failed to remove the NPC as killed");
      return;
    }

    if (EasyNPCEntityHandler.spawn(entityUUID, helper.getLevel())) {
      helper.fail("Killed NPC " + entityUUID + " must not spawn without force");
      return;
    }

    if (!EasyNPCEntityHandler.spawn(entityUUID, helper.getLevel(), true)) {
      helper.fail("Killed NPC " + entityUUID + " must spawn with force");
    }
  }

  public static void assertKilledNPCCanBeRevived(GameTestHelper helper, EntityType<?> entityType) {
    UUID entityUUID = despawnAsKilled(helper, entityType);
    if (EasyNPCEntityHandler.spawn(entityUUID, helper.getLevel())) {
      helper.fail("Killed NPC " + entityUUID + " must not spawn without force");
      return;
    }

    Optional<String> reviveRejection = EasyNPCEntityHandler.getReviveRejection(entityUUID);
    if (reviveRejection.isPresent()) {
      helper.fail("Killed NPC " + entityUUID + " is rejected for revive: " + reviveRejection.get());
      return;
    }

    if (!EasyNPCEntityHandler.revive(entityUUID, helper.getLevel())) {
      helper.fail("Killed NPC " + entityUUID + " was not revived");
      return;
    }

    GameTestHelpers.assertNotNull(
        helper,
        "Revived NPC " + entityUUID + " was not found in the world",
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel()));
    GameTestHelpers.assertEquals(
        helper,
        "Revived NPC is still stored as removed",
        NPCRemovalReason.NONE,
        getStoredRemovalReason(helper, entityUUID));
  }

  public static void assertReviveUsesGivenPosition(
      GameTestHelper helper, EntityType<?> entityType) {
    UUID entityUUID = despawnAsKilled(helper, entityType);
    Vec3 revivePosition = helper.absoluteVec(new Vec3(2.5, 2, 2.5));
    if (!EasyNPCEntityHandler.revive(entityUUID, helper.getLevel(), revivePosition)) {
      helper.fail("Killed NPC " + entityUUID + " was not revived at " + revivePosition);
      return;
    }

    EasyNPC<?> revivedNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
    if (revivedNPC == null) {
      helper.fail("Revived NPC " + entityUUID + " was not found in the world");
      return;
    }

    Vec3 revivedPosition = revivedNPC.getEntity().position();
    if (revivedPosition.distanceToSqr(revivePosition) > POSITION_TOLERANCE) {
      helper.fail("Revived NPC is at " + revivedPosition + " instead of " + revivePosition);
    }
  }

  public static void assertRevivedNPCHasFullHealth(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(easyNPC);
    UUID entityUUID = easyNPC.getEntityUUID();
    LivingEntity livingEntity = easyNPC.getLivingEntity();
    livingEntity.kill();
    GameTestHelpers.assertTrue(helper, "NPC survived the kill", livingEntity.isDeadOrDying());

    helper
        .startSequence()
        .thenWaitUntil(
            () ->
                GameTestHelpers.assertTrue(
                    helper, "Dead NPC was not removed from the world", livingEntity.isRemoved()))
        .thenExecute(
            () -> {
              GameTestHelpers.assertEquals(
                  helper,
                  "Dead NPC was not stored as killed",
                  NPCRemovalReason.KILLED,
                  getStoredRemovalReason(helper, entityUUID));
              GameTestHelpers.assertTrue(
                  helper,
                  "Dead NPC " + entityUUID + " was not revived",
                  EasyNPCEntityHandler.revive(entityUUID, helper.getLevel()));

              EasyNPC<?> revivedNPC =
                  LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
              GameTestHelpers.assertNotNull(
                  helper, "Revived NPC " + entityUUID + " was not found in the world", revivedNPC);
              LivingEntity revivedEntity = revivedNPC.getLivingEntity();
              GameTestHelpers.assertTrue(
                  helper,
                  "Revived NPC is still dying",
                  !revivedEntity.isDeadOrDying() && revivedEntity.deathTime == 0);
              GameTestHelpers.assertEquals(
                  helper,
                  "Revived NPC does not have full health",
                  revivedEntity.getMaxHealth(),
                  revivedEntity.getHealth());
            })
        .thenSucceed();
  }

  public static void assertReviveRejectsDeletedNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(easyNPC);
    UUID entityUUID = easyNPC.getEntityUUID();
    GameTestHelpers.addCleanup(helper, () -> NPCEntityManager.clearNPCDeleted(entityUUID));
    if (!EasyNPCEntityHandler.delete(easyNPC)) {
      helper.fail("Failed to delete the NPC");
      return;
    }

    if (EasyNPCEntityHandler.revive(entityUUID, helper.getLevel())) {
      helper.fail("Deleted NPC " + entityUUID + " was revived");
    }
  }

  public static void assertReviveRejectsLoadedNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(easyNPC);

    if (EasyNPCEntityHandler.revive(easyNPC.getEntityUUID(), helper.getLevel())) {
      helper.fail("Loaded NPC " + easyNPC.getEntityUUID() + " was revived a second time");
    }
  }

  public static void assertDefaultDataAppliedOnEveryCreationPath(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerLevel serverLevel = helper.getLevel();
    BlockPos spawnPosition = helper.absolutePos(new BlockPos(1, 2, 1));
    Entity spawnedEntity = entityType.spawn(serverLevel, spawnPosition, MobSpawnType.SPAWN_EGG);
    if (!(spawnedEntity instanceof EasyNPC<?> spawnedNPC)) {
      helper.fail("Spawn egg path did not create an NPC for " + entityType);
      return;
    }

    UUID entityUUID = spawnedNPC.getEntityUUID();
    GameTestHelpers.removeNPCDataAfterTest(helper, entityUUID);
    ObjectiveDataCapable<?> objectiveData = spawnedNPC.getEasyNPCObjectiveData();
    GameTestHelpers.assertTrue(
        helper,
        "Spawn egg path did not register the default objectives",
        objectiveData.hasObjective(ObjectiveType.LOOK_AT_PLAYER)
            && objectiveData.hasObjective(ObjectiveType.LOOK_AT_MOB));

    objectiveData.removeCustomObjective(ObjectiveType.LOOK_AT_MOB);
    NPCEntityManager.saveNPC(spawnedNPC);
    spawnedNPC
        .getMob()
        .finalizeSpawn(
            serverLevel,
            serverLevel.getCurrentDifficultyAt(spawnPosition),
            MobSpawnType.SPAWN_EGG,
            null,
            null);
    assertDefaultDataNotReapplied(helper, "a second finalize spawn", spawnedNPC);

    GameTestHelpers.assertTrue(
        helper,
        "Failed to despawn the NPC",
        EasyNPCEntityHandler.despawn(spawnedNPC, NPCRemovalReason.DESPAWNED));
    GameTestHelpers.assertTrue(
        helper, "Failed to respawn the NPC", EasyNPCEntityHandler.spawn(entityUUID, serverLevel));
    EasyNPC<?> respawnedNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, serverLevel);
    assertDefaultDataNotReapplied(helper, "spawn", respawnedNPC);

    GameTestHelpers.assertTrue(
        helper,
        "Failed to remove the NPC as killed",
        EasyNPCEntityHandler.despawn(respawnedNPC, NPCRemovalReason.KILLED));
    GameTestHelpers.assertTrue(
        helper, "Failed to revive the NPC", EasyNPCEntityHandler.revive(entityUUID, serverLevel));
    assertDefaultDataNotReapplied(
        helper, "revive", LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, serverLevel));
  }

  private static void assertDefaultDataNotReapplied(
      GameTestHelper helper, String creationPath, EasyNPC<?> easyNPC) {
    GameTestHelpers.assertNotNull(helper, "No NPC in the world after " + creationPath, easyNPC);
    GameTestHelpers.assertTrue(
        helper,
        "NPC is not finalized after " + creationPath,
        easyNPC.getEasyNPCStatusData().getStatusDataFlag(StatusDataType.FINALIZED));

    ObjectiveDataCapable<?> objectiveData = easyNPC.getEasyNPCObjectiveData();
    GameTestHelpers.assertTrue(
        helper,
        "Default objective is missing after " + creationPath,
        objectiveData.hasObjective(ObjectiveType.LOOK_AT_PLAYER));
    GameTestHelpers.assertTrue(
        helper,
        "Removed default objective returned after " + creationPath,
        !objectiveData.hasObjective(ObjectiveType.LOOK_AT_MOB));
  }

  private static UUID despawnAsKilled(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(easyNPC);
    GameTestHelpers.assertTrue(
        helper,
        "Failed to remove the NPC as killed",
        EasyNPCEntityHandler.despawn(easyNPC, NPCRemovalReason.KILLED));
    return easyNPC.getEntityUUID();
  }

  public static void assertLoadedNPCIsNotSpawnedTwice(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(easyNPC);

    if (EasyNPCEntityHandler.spawn(easyNPC.getEntityUUID(), helper.getLevel(), true)) {
      helper.fail("Loaded NPC " + easyNPC.getEntityUUID() + " was spawned a second time");
    }
  }

  public static void assertActionGroupsAreRemovedOnLeave(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(2, 2, 1), "action-group-player");

    ActionManager.addPlayer(easyNPC.getMob(), ActionGroup.DISTANCE_NEAR, serverPlayer);
    ActionManager.removeServerPlayer(serverPlayer);
    if (ActionManager.containsPlayer(easyNPC.getMob(), ActionGroup.DISTANCE_NEAR, serverPlayer)) {
      helper.fail("Action groups still reference the player after the player left");
      return;
    }

    ActionManager.addPlayer(easyNPC.getMob(), ActionGroup.DISTANCE_NEAR, serverPlayer);
    ActionManager.removeMob(easyNPC.getMob());
    if (ActionManager.containsPlayer(easyNPC.getMob(), ActionGroup.DISTANCE_NEAR, serverPlayer)) {
      helper.fail("Action groups still reference the NPC after the NPC left");
    }
  }

  public static void assertEasyNPCEntityTypesAreRecognized(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerLevel serverLevel = helper.getLevel();
    if (!EasyNPCEntityTypes.isEasyNPCEntityType(entityType, serverLevel)) {
      helper.fail(entityType + " must be recognized as an Easy NPC type");
      return;
    }

    if (EasyNPCEntityTypes.isEasyNPCEntityType(EntityType.PIG, serverLevel)) {
      helper.fail("Pig must not be recognized as an Easy NPC type");
      return;
    }

    CompoundTag easyNPCTag = new CompoundTag();
    easyNPCTag.putString("id", EntityType.getKey(entityType).toString());
    if (!EasyNPCEntityTypes.isEasyNPCEntityTag(easyNPCTag, serverLevel)) {
      helper.fail("Entity tag of " + entityType + " must be recognized as an Easy NPC");
      return;
    }

    CompoundTag pigTag = new CompoundTag();
    pigTag.putString("id", EntityType.getKey(EntityType.PIG).toString());
    if (EasyNPCEntityTypes.isEasyNPCEntityTag(pigTag, serverLevel)) {
      helper.fail("Entity tag of a pig must not be recognized as an Easy NPC");
    }
  }
}
