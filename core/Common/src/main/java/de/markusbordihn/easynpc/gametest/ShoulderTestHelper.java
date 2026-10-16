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

import de.markusbordihn.easynpc.api.event.EasyNPCEventRegistry;
import de.markusbordihn.easynpc.api.event.ShoulderEventListener;
import de.markusbordihn.easynpc.api.handler.EasyNPCAttributeHandler;
import de.markusbordihn.easynpc.api.handler.EasyNPCEntityHandler;
import de.markusbordihn.easynpc.api.handler.EasyNPCObjectiveHandler;
import de.markusbordihn.easynpc.api.handler.EasyNPCShoulderHandler;
import de.markusbordihn.easynpc.data.attribute.InteractionAttributeType;
import de.markusbordihn.easynpc.data.npc.NPCRemovalReason;
import de.markusbordihn.easynpc.data.npc.ShoulderSide;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.LivingEntityManager;
import de.markusbordihn.easynpc.entity.NPCEntityManager;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.SitOnOwnerShoulderGoal;
import java.util.Optional;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicReference;
import net.minecraft.core.BlockPos;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.network.protocol.game.ServerboundPlayerLoadedPacket;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.server.players.PlayerList;
import net.minecraft.util.ProblemReporter;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.EntitySpawnReason;
import net.minecraft.world.entity.EntitySpawnRequest;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EntityTypes;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.animal.parrot.Parrot;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.level.block.Blocks;
import net.minecraft.world.level.storage.TagValueInput;
import net.minecraft.world.level.storage.TagValueOutput;
import net.minecraft.world.phys.Vec3;

public class ShoulderTestHelper {

  public static final int REMOUNT_COOLDOWN_TICKS = 20;
  public static final int NO_PROGRESS_TEST_TIMEOUT_TICKS =
      SitOnOwnerShoulderGoal.MAX_TICKS_WITHOUT_PROGRESS * 2;
  private static final int OBJECTIVE_COOLDOWN_TICKS = 40;
  private static final int VANILLA_SHOULDER_RELEASE_DELAY_TICKS = 25;
  private static final int TICK_COUNT_AFTER_DEFAULT_COOLDOWN =
      ObjectiveDataEntry.DEFAULT_COOLDOWN_TICKS + 1;
  private static final float MAX_DISTANCE = 4.0F;
  private static final double POSITION_TOLERANCE = 1.0E-6D;

  private ShoulderTestHelper() {}

  private static ServerPlayer mockGroundedPlayer(GameTestHelper helper, String playerName) {
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(2, 2, 1), playerName);
    serverPlayer.setOnGround(true);
    return serverPlayer;
  }

  private static EasyNPC<?> mockStoredNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(1, 2, 1));
    NPCEntityManager.saveNPC(easyNPC);
    return easyNPC;
  }

  private static Optional<NPCRemovalReason> getStoredRemovalReason(UUID entityUUID) {
    return NPCEntityManager.getNPC(entityUUID).map(entry -> entry.metadata().removalReason());
  }

  public static void assertSitOnShoulderStoresNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockGroundedPlayer(helper, "shoulder-sit-player");
    UUID entityUUID = easyNPC.getEntityUUID();

    if (!EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer)) {
      helper.fail("NPC " + entityUUID + " did not sit on the shoulder of a grounded player");
      return;
    }

    if (!easyNPC.getEntity().isRemoved()) {
      helper.fail("NPC is still in the world while it sits on the shoulder");
      return;
    }

    Optional<UUID> shoulderUUID =
        EasyNPCShoulderHandler.getShoulderNPCUUID(serverPlayer, ShoulderSide.LEFT);
    if (shoulderUUID.isEmpty() || !shoulderUUID.get().equals(entityUUID)) {
      helper.fail("Left shoulder holds " + shoulderUUID + " instead of NPC " + entityUUID);
      return;
    }

    Optional<NPCRemovalReason> removalReason = getStoredRemovalReason(entityUUID);
    if (removalReason.isEmpty() || removalReason.get() != NPCRemovalReason.ON_SHOULDER) {
      helper.fail("NPC on the shoulder is stored with the removal reason " + removalReason);
      return;
    }

    if (!EasyNPCShoulderHandler.isOnShoulder(entityUUID)) {
      helper.fail("NPC " + entityUUID + " is not reported as sitting on a shoulder");
      return;
    }

    if (EasyNPCEntityHandler.spawn(entityUUID, helper.getLevel())) {
      helper.fail("NPC on the shoulder was spawned a second time");
    }
  }

  public static void assertSecondNPCUsesRightShoulder(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> leftNPC = mockStoredNPC(helper, entityType);
    EasyNPC<?> rightNPC = mockStoredNPC(helper, entityType);
    EasyNPC<?> thirdNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockGroundedPlayer(helper, "shoulder-both-player");

    if (!EasyNPCShoulderHandler.sitOnShoulder(leftNPC, serverPlayer)
        || !EasyNPCShoulderHandler.sitOnShoulder(rightNPC, serverPlayer)) {
      helper.fail("Two NPCs did not sit on both shoulders");
      return;
    }

    Optional<UUID> rightUUID =
        EasyNPCShoulderHandler.getShoulderNPCUUID(serverPlayer, ShoulderSide.RIGHT);
    if (rightUUID.isEmpty() || !rightUUID.get().equals(rightNPC.getEntityUUID())) {
      helper.fail("Right shoulder holds " + rightUUID + " instead of the second NPC");
      return;
    }

    if (EasyNPCShoulderHandler.sitOnShoulder(thirdNPC, serverPlayer)
        || thirdNPC.getEntity().isRemoved()) {
      helper.fail("A third NPC sat on a player whose shoulders are both taken");
    }
  }

  public static void assertDismountRespawnsNPC(GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockGroundedPlayer(helper, "shoulder-dismount-player");
    UUID entityUUID = easyNPC.getEntityUUID();
    AtomicReference<UUID> dismountedUUID = new AtomicReference<>();
    ShoulderEventListener shoulderEventListener =
        new ShoulderEventListener() {
          @Override
          public void onShoulderMounted(
              EasyNPC<?> mountedNPC, Player player, ShoulderSide shoulderSide) {}

          @Override
          public void onShoulderDismounted(EasyNPC<?> dismountedNPC) {
            dismountedUUID.set(dismountedNPC.getEntityUUID());
          }
        };
    EasyNPCEventRegistry.registerShoulderEventListener(shoulderEventListener);
    GameTestHelpers.addCleanup(
        helper, () -> EasyNPCEventRegistry.unregisterShoulderEventListener(shoulderEventListener));

    if (!EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer)) {
      helper.fail("NPC " + entityUUID + " did not sit on the shoulder");
      return;
    }

    if (!EasyNPCShoulderHandler.dismount(serverPlayer, ShoulderSide.LEFT)) {
      helper.fail("NPC " + entityUUID + " was not dismounted from the left shoulder");
      return;
    }

    if (!serverPlayer.getShoulderEntityLeft().isEmpty()) {
      helper.fail("Left shoulder still holds the NPC after the dismount");
      return;
    }

    if (LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel()) == null) {
      helper.fail("Dismounted NPC " + entityUUID + " was not found in the world");
      return;
    }

    Optional<NPCRemovalReason> removalReason = getStoredRemovalReason(entityUUID);
    if (removalReason.isEmpty() || removalReason.get() != NPCRemovalReason.NONE) {
      helper.fail("Dismounted NPC still uses the removal reason " + removalReason);
      return;
    }

    if (!entityUUID.equals(dismountedUUID.get())) {
      helper.fail("Dismount event was not fired for NPC " + entityUUID);
    }
  }

  public static void assertShoulderSizeLimit(
      GameTestHelper helper, EntityType<?> smallEntityType, EntityType<?> largeEntityType) {
    EasyNPC<?> smallNPC = mockStoredNPC(helper, smallEntityType);
    EasyNPC<?> largeNPC = mockStoredNPC(helper, largeEntityType);

    if (!SitOnOwnerShoulderGoal.fitsOnShoulder(smallNPC.getEntity())) {
      helper.fail("Small NPC " + smallEntityType + " does not fit on a shoulder");
      return;
    }

    if (SitOnOwnerShoulderGoal.fitsOnShoulder(largeNPC.getEntity())) {
      helper.fail("Large NPC " + largeEntityType + " fits on a shoulder");
    }
  }

  public static void assertAttributeTogglesObjective(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);

    EasyNPCAttributeHandler.setInteractionAttribute(
        easyNPC, InteractionAttributeType.CAN_SIT_ON_SHOULDER, true);
    if (!EasyNPCObjectiveHandler.hasObjective(easyNPC, ObjectiveType.SIT_ON_OWNER_SHOULDER)) {
      helper.fail("Enabling \"Sit on Shoulder\" did not add the shoulder objective");
      return;
    }

    EasyNPCAttributeHandler.setInteractionAttribute(
        easyNPC, InteractionAttributeType.CAN_SIT_ON_SHOULDER, false);
    if (EasyNPCObjectiveHandler.hasObjective(easyNPC, ObjectiveType.SIT_ON_OWNER_SHOULDER)) {
      helper.fail("Disabling \"Sit on Shoulder\" did not remove the shoulder objective");
    }
  }

  public static void assertDismountKeepsNPCWhenUUIDTaken(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockCenteredPlayer(helper, "shoulder-taken-uuid-player");
    UUID entityUUID = easyNPC.getEntityUUID();
    if (!EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer)) {
      helper.fail("NPC " + entityUUID + " did not sit on the shoulder");
      return;
    }

    Mob duplicate = EntityTypes.PIG.create(helper.getLevel(), EntitySpawnReason.LOAD);
    duplicate.setUUID(entityUUID);
    duplicate.setNoAi(true);
    duplicate.setPos(helper.absoluteVec(new Vec3(0.5, 2, 0.5)));
    if (!helper.getLevel().addFreshEntity(duplicate)) {
      helper.fail("Unable to occupy the UUID " + entityUUID + " with another entity");
      return;
    }
    GameTestHelpers.addCleanup(helper, duplicate::discard);

    if (EasyNPCShoulderHandler.dismount(serverPlayer, ShoulderSide.LEFT)) {
      helper.fail("Dismount succeeded although the UUID " + entityUUID + " is taken");
      return;
    }

    assertStillOnLeftShoulder(helper, serverPlayer, entityUUID);
  }

  public static void assertDismountKeepsNPCWhenBlocked(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockCenteredPlayer(helper, "shoulder-blocked-player");
    UUID entityUUID = easyNPC.getEntityUUID();
    if (!EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer)) {
      helper.fail("NPC " + entityUUID + " did not sit on the shoulder");
      return;
    }

    helper.setBlock(new BlockPos(1, 2, 1), Blocks.STONE);
    helper.setBlock(new BlockPos(1, 3, 1), Blocks.STONE);
    if (EasyNPCShoulderHandler.dismount(serverPlayer, ShoulderSide.LEFT)) {
      helper.fail("Dismount succeeded although every dismount position is blocked");
      return;
    }

    if (LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel()) != null) {
      helper.fail("NPC " + entityUUID + " was put into a blocked position");
      return;
    }

    assertStillOnLeftShoulder(helper, serverPlayer, entityUUID);
  }

  public static void assertDismountFallsBackToPlayerPosition(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockCenteredPlayer(helper, "shoulder-low-ceiling-player");
    UUID entityUUID = easyNPC.getEntityUUID();
    if (!EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer)) {
      helper.fail("NPC " + entityUUID + " did not sit on the shoulder");
      return;
    }

    helper.setBlock(new BlockPos(1, 3, 1), Blocks.STONE);
    if (!EasyNPCShoulderHandler.dismount(serverPlayer, ShoulderSide.LEFT)) {
      helper.fail("NPC " + entityUUID + " was not dismounted below a low ceiling");
      return;
    }

    EasyNPC<?> dismountedNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
    if (dismountedNPC == null) {
      helper.fail("Dismounted NPC " + entityUUID + " was not found in the world");
      return;
    }

    double dismountedY = dismountedNPC.getEntity().getY();
    if (Math.abs(dismountedY - serverPlayer.getY()) > POSITION_TOLERANCE) {
      helper.fail(
          "NPC was put down at y=" + dismountedY + " instead of the player y=" + serverPlayer.getY());
    }
  }

  public static void assertOwnerDismountBlocksRemount(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockCenteredPlayer(helper, "shoulder-remount-player");
    easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(serverPlayer.getUUID());
    Optional<EasyNPC<?>> dismountedNPC = sitAndDismountWithCooldown(helper, easyNPC, serverPlayer);
    if (dismountedNPC.isEmpty()) {
      return;
    }

    prepareForShoulderGoal(dismountedNPC.get());
    SitOnOwnerShoulderGoal goal =
        new SitOnOwnerShoulderGoal(
            dismountedNPC.get(),
            new ObjectiveDataEntry(ObjectiveType.SIT_ON_OWNER_SHOULDER).setCooldownTicks(0));
    if (goal.canUse()) {
      helper.fail("NPC wants to sit on the shoulder again during the remount cooldown");
      return;
    }

    helper.runAfterDelay(
        REMOUNT_COOLDOWN_TICKS + 1,
        () -> {
          if (!goal.canUse()) {
            helper.fail("NPC does not want to sit on the shoulder after the remount cooldown");
            return;
          }

          helper.succeed();
        });
  }

  public static void assertRemountCooldownSurvivesReload(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockCenteredPlayer(helper, "shoulder-reload-player");
    Optional<EasyNPC<?>> dismountedNPC = sitAndDismountWithCooldown(helper, easyNPC, serverPlayer);
    if (dismountedNPC.isEmpty()) {
      return;
    }

    TagValueOutput entityOutput = createValueOutput(helper);
    dismountedNPC.get().getEntity().save(entityOutput);
    Optional<Entity> reloadedEntity =
        EntityType.create(
            TagValueInput.create(
                ProblemReporter.DISCARDING,
                helper.getLevel().registryAccess(),
                entityOutput.buildResult()),
            helper.getLevel(),
            new EntitySpawnRequest(EntitySpawnReason.LOAD, false));
    if (reloadedEntity.isEmpty() || !(reloadedEntity.get() instanceof EasyNPC<?> reloadedNPC)) {
      helper.fail("Dismounted NPC could not be loaded from its saved data");
      return;
    }

    long expectedGameTime = EasyNPCShoulderHandler.getRemountAllowedGameTime(dismountedNPC.get());
    long reloadedGameTime = EasyNPCShoulderHandler.getRemountAllowedGameTime(reloadedNPC);
    if (reloadedGameTime != expectedGameTime) {
      helper.fail(
          "Reloaded NPC may remount at " + reloadedGameTime + " instead of " + expectedGameTime);
    }
  }

  public static void assertShoulderCooldownFromObjective(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = mockCenteredPlayer(helper, "shoulder-objective-cooldown-player");
    EasyNPC<?> easyNPC = mockOwnedNPC(helper, entityType, serverPlayer);
    SitOnOwnerShoulderGoal goal =
        new SitOnOwnerShoulderGoal(
            easyNPC,
            new ObjectiveDataEntry(ObjectiveType.SIT_ON_OWNER_SHOULDER)
                .setCooldownTicks(OBJECTIVE_COOLDOWN_TICKS));

    easyNPC.getMob().tickCount = OBJECTIVE_COOLDOWN_TICKS;
    if (goal.canUse()) {
      helper.fail("NPC wants to sit on the shoulder before the objective cooldown elapsed");
      return;
    }

    easyNPC.getMob().tickCount = OBJECTIVE_COOLDOWN_TICKS + 1;
    if (!goal.canUse()) {
      helper.fail("NPC does not want to sit on the shoulder after the objective cooldown");
    }
  }

  public static void assertShoulderGoalRespectsMaxDistance(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = mockCenteredPlayer(helper, "shoulder-max-distance-player");
    EasyNPC<?> easyNPC = mockOwnedNPC(helper, entityType, serverPlayer);
    SitOnOwnerShoulderGoal goal =
        new SitOnOwnerShoulderGoal(
            easyNPC,
            new ObjectiveDataEntry(ObjectiveType.SIT_ON_OWNER_SHOULDER)
                .setStartDistance(MAX_DISTANCE)
                .setCooldownTicks(OBJECTIVE_COOLDOWN_TICKS));
    Vec3 nearPosition = serverPlayer.position();
    Vec3 farPosition = nearPosition.add(MAX_DISTANCE + 1.0D, 0.0D, 0.0D);

    serverPlayer.setPos(farPosition);
    if (goal.canUse()) {
      helper.fail("NPC starts to chase an owner beyond its max distance");
      return;
    }

    serverPlayer.setPos(nearPosition);
    if (!goal.canUse()) {
      helper.fail("NPC does not start to chase an owner within its max distance");
      return;
    }

    goal.start();
    serverPlayer.setPos(farPosition);
    if (goal.canContinueToUse()) {
      helper.fail("NPC keeps chasing an owner who moved beyond its max distance");
      return;
    }

    goal.stop();
    serverPlayer.setPos(nearPosition);
    if (goal.canUse()) {
      helper.fail("NPC chases its owner again before the cooldown of the aborted chase");
    }
  }

  public static void assertShoulderGoalStopsWithoutProgress(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = mockCenteredPlayer(helper, "shoulder-no-progress-player");
    EasyNPC<?> easyNPC = mockOwnedNPC(helper, entityType, serverPlayer);
    SitOnOwnerShoulderGoal goal =
        new SitOnOwnerShoulderGoal(
            easyNPC,
            new ObjectiveDataEntry(ObjectiveType.SIT_ON_OWNER_SHOULDER)
                .setCooldownTicks(OBJECTIVE_COOLDOWN_TICKS));
    if (!goal.canUse()) {
      helper.fail("NPC does not start to chase its owner");
      return;
    }

    goal.start();
    helper.runAfterDelay(
        SitOnOwnerShoulderGoal.MAX_TICKS_WITHOUT_PROGRESS / 2,
        () -> {
          if (!goal.canContinueToUse()) {
            helper.fail("NPC gave up the chase before the progress timeout");
          }
        });
    helper.runAfterDelay(
        SitOnOwnerShoulderGoal.MAX_TICKS_WITHOUT_PROGRESS + 1,
        () -> {
          if (goal.canContinueToUse()) {
            helper.fail("NPC keeps chasing its owner without getting closer");
            return;
          }

          goal.stop();
          if (goal.canUse()) {
            helper.fail("NPC chases its owner again before the cooldown of the aborted chase");
            return;
          }

          helper.succeed();
        });
  }

  public static void assertShoulderGoalWaitsOutOfCombat(
      GameTestHelper helper, EntityType<?> entityType) {
    ServerPlayer serverPlayer = mockCenteredPlayer(helper, "shoulder-combat-player");
    EasyNPC<?> easyNPC = mockOwnedNPC(helper, entityType, serverPlayer);
    Mob attacker = GameTestHelpers.spawnEntityType(helper, EntityTypes.ZOMBIE);
    attacker.setNoAi(true);
    attacker.setPos(helper.absoluteVec(new Vec3(0.5, 2, 0.5)));
    SitOnOwnerShoulderGoal outOfCombatGoal =
        new SitOnOwnerShoulderGoal(
            easyNPC,
            new ObjectiveDataEntry(ObjectiveType.SIT_ON_OWNER_SHOULDER).setOnlyOutOfCombat(true));
    SitOnOwnerShoulderGoal defaultGoal =
        new SitOnOwnerShoulderGoal(
            easyNPC, new ObjectiveDataEntry(ObjectiveType.SIT_ON_OWNER_SHOULDER));

    serverPlayer.setLastHurtByMob(attacker);
    if (outOfCombatGoal.canUse()) {
      helper.fail("NPC wants to sit on the shoulder of an owner who was just hurt");
      return;
    }

    if (!defaultGoal.canUse()) {
      helper.fail("NPC without \"only out of combat\" refuses an owner who was just hurt");
      return;
    }

    serverPlayer.setLastHurtByMob(null);
    serverPlayer.setLastHurtMob(attacker);
    if (outOfCombatGoal.canUse()) {
      helper.fail("NPC wants to sit on the shoulder of an owner who just attacked");
    }
  }

  public static void assertReviveRejectsNPCOnShoulder(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockGroundedPlayer(helper, "shoulder-revive-player");
    UUID entityUUID = easyNPC.getEntityUUID();
    if (!EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer)) {
      helper.fail("NPC " + entityUUID + " did not sit on the shoulder of a grounded player");
      return;
    }

    if (EasyNPCEntityHandler.revive(entityUUID, helper.getLevel())) {
      helper.fail("NPC on the shoulder was revived a second time");
      return;
    }

    assertStillOnLeftShoulder(helper, serverPlayer, entityUUID);
  }

  public static void assertVanillaShoulderReleaseRespawnsNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockCenteredPlayer(helper, "shoulder-vanilla-release-player");
    UUID entityUUID = easyNPC.getEntityUUID();
    if (!EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer)) {
      helper.fail("NPC " + entityUUID + " did not sit on the shoulder");
      return;
    }

    helper.runAfterDelay(
        VANILLA_SHOULDER_RELEASE_DELAY_TICKS,
        () -> {
          releaseShoulderEntitiesByFlying(serverPlayer);
          if (LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel())
              == null) {
            helper.fail("NPC " + entityUUID + " was not put down by the vanilla shoulder release");
            return;
          }

          if (!serverPlayer.getShoulderEntityLeft().isEmpty()) {
            helper.fail("Left shoulder still holds data after the NPC was put down");
            return;
          }

          if (EasyNPCShoulderHandler.isOnShoulder(entityUUID)) {
            helper.fail("NPC " + entityUUID + " is still stored as sitting on a shoulder");
            return;
          }

          helper.succeed();
        });
  }

  public static void assertVanillaShoulderReleaseKeepsBlockedNPC(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockCenteredPlayer(helper, "shoulder-vanilla-blocked-player");
    UUID entityUUID = easyNPC.getEntityUUID();
    if (!EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer)) {
      helper.fail("NPC " + entityUUID + " did not sit on the shoulder");
      return;
    }

    Parrot parrot = EntityTypes.PARROT.create(helper.getLevel(), EntitySpawnReason.LOAD);
    TagValueOutput parrotOutput = createValueOutput(helper);
    if (parrot == null
        || !parrot.save(parrotOutput)
        || !serverPlayer.setEntityOnShoulder(parrotOutput.buildResult())) {
      helper.fail("Unable to put a parrot on the right shoulder");
      return;
    }
    UUID parrotUUID = parrot.getUUID();
    GameTestHelpers.addCleanup(
        helper,
        () -> {
          Entity releasedParrot = helper.getLevel().getEntity(parrotUUID);
          if (releasedParrot != null) {
            releasedParrot.discard();
          }
        });

    helper.setBlock(new BlockPos(1, 2, 1), Blocks.STONE);
    helper.setBlock(new BlockPos(1, 3, 1), Blocks.STONE);
    helper.runAfterDelay(
        VANILLA_SHOULDER_RELEASE_DELAY_TICKS,
        () -> {
          releaseShoulderEntitiesByFlying(serverPlayer);
          if (LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel())
              != null) {
            helper.fail("NPC " + entityUUID + " was put into a blocked position");
            return;
          }

          if (!serverPlayer.getShoulderEntityRight().isEmpty()) {
            helper.fail("Parrot on the right shoulder was not released");
            return;
          }

          assertStillOnLeftShoulder(helper, serverPlayer, entityUUID);
          helper.succeed();
        });
  }

  public static void assertPlayerDeathPutsNPCDown(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockCenteredPlayer(helper, "shoulder-death-player");
    UUID entityUUID = easyNPC.getEntityUUID();
    if (!EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer)) {
      helper.fail("NPC " + entityUUID + " did not sit on the shoulder");
      return;
    }

    helper.runAfterDelay(
        VANILLA_SHOULDER_RELEASE_DELAY_TICKS,
        () -> {
          serverPlayer.kill(helper.getLevel());
          if (LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel())
              == null) {
            helper.fail("NPC " + entityUUID + " was not put down when its player died");
            return;
          }

          if (EasyNPCShoulderHandler.isOnShoulder(entityUUID)) {
            helper.fail("NPC " + entityUUID + " is still stored as sitting on a shoulder");
            return;
          }

          helper.succeed();
        });
  }

  public static void assertBlockedNPCStaysOnShoulderAfterRespawn(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    ServerPlayer serverPlayer = mockCenteredPlayer(helper, "shoulder-respawn-player");
    UUID entityUUID = easyNPC.getEntityUUID();
    if (!EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer)) {
      helper.fail("NPC " + entityUUID + " did not sit on the shoulder");
      return;
    }

    helper.setBlock(new BlockPos(1, 2, 1), Blocks.STONE);
    helper.setBlock(new BlockPos(1, 3, 1), Blocks.STONE);
    helper.runAfterDelay(
        VANILLA_SHOULDER_RELEASE_DELAY_TICKS,
        () -> {
          serverPlayer.kill(helper.getLevel());
          PlayerList playerList = helper.getLevel().getServer().getPlayerList();
          ServerPlayer respawnedPlayer =
              playerList.respawn(serverPlayer, false, Entity.RemovalReason.KILLED);
          GameTestHelpers.addCleanup(helper, () -> playerList.remove(respawnedPlayer));
          if (LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel())
              != null) {
            helper.fail("NPC " + entityUUID + " was put into a blocked position");
            return;
          }

          assertStillOnLeftShoulder(helper, respawnedPlayer, entityUUID);
          helper.succeed();
        });
  }

  private static TagValueOutput createValueOutput(GameTestHelper helper) {
    return TagValueOutput.createWithContext(
        ProblemReporter.DISCARDING, helper.getLevel().registryAccess());
  }

  private static void releaseShoulderEntitiesByFlying(ServerPlayer serverPlayer) {
    serverPlayer.getAbilities().flying = true;
    serverPlayer.aiStep();
  }

  private static ServerPlayer mockCenteredPlayer(GameTestHelper helper, String playerName) {
    ServerPlayer serverPlayer =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(1.5, 2, 1.5), playerName);
    serverPlayer.setOnGround(true);
    // Vanilla 1.21.4+ ServerPlayer is invulnerable, even to kill(), until its client has loaded.
    serverPlayer.connection.handleAcceptPlayerLoad(new ServerboundPlayerLoadedPacket());
    return serverPlayer;
  }

  private static EasyNPC<?> mockOwnedNPC(
      GameTestHelper helper, EntityType<?> entityType, ServerPlayer owner) {
    EasyNPC<?> easyNPC = mockStoredNPC(helper, entityType);
    easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(owner.getUUID());
    prepareForShoulderGoal(easyNPC);
    return easyNPC;
  }

  private static void prepareForShoulderGoal(EasyNPC<?> easyNPC) {
    easyNPC.getMob().setNoAi(true);
    easyNPC.getMob().tickCount = TICK_COUNT_AFTER_DEFAULT_COOLDOWN;
  }

  private static Optional<EasyNPC<?>> sitAndDismountWithCooldown(
      GameTestHelper helper, EasyNPC<?> easyNPC, ServerPlayer serverPlayer) {
    UUID entityUUID = easyNPC.getEntityUUID();
    if (!EasyNPCShoulderHandler.sitOnShoulder(easyNPC, serverPlayer)) {
      helper.fail("NPC " + entityUUID + " did not sit on the shoulder");
      return Optional.empty();
    }

    long dismountGameTime = helper.getLevel().getGameTime();
    if (!EasyNPCShoulderHandler.dismount(
        serverPlayer, ShoulderSide.LEFT, REMOUNT_COOLDOWN_TICKS)) {
      helper.fail("NPC " + entityUUID + " was not dismounted with a remount cooldown");
      return Optional.empty();
    }

    EasyNPC<?> dismountedNPC =
        LivingEntityManager.getServerEasyNPCEntityByUUID(entityUUID, helper.getLevel());
    if (dismountedNPC == null) {
      helper.fail("Dismounted NPC " + entityUUID + " was not found in the world");
      return Optional.empty();
    }

    long remountAllowedGameTime = EasyNPCShoulderHandler.getRemountAllowedGameTime(dismountedNPC);
    if (remountAllowedGameTime != dismountGameTime + REMOUNT_COOLDOWN_TICKS) {
      helper.fail(
          "NPC may remount at "
              + remountAllowedGameTime
              + " instead of "
              + (dismountGameTime + REMOUNT_COOLDOWN_TICKS));
      return Optional.empty();
    }

    return Optional.of(dismountedNPC);
  }

  private static void assertStillOnLeftShoulder(
      GameTestHelper helper, ServerPlayer serverPlayer, UUID entityUUID) {
    Optional<UUID> shoulderUUID =
        EasyNPCShoulderHandler.getShoulderNPCUUID(serverPlayer, ShoulderSide.LEFT);
    if (shoulderUUID.isEmpty() || !shoulderUUID.get().equals(entityUUID)) {
      helper.fail("Left shoulder lost NPC " + entityUUID);
      return;
    }

    if (!EasyNPCShoulderHandler.isOnShoulder(entityUUID)) {
      helper.fail("NPC " + entityUUID + " is no longer stored as sitting on a shoulder");
    }
  }
}
