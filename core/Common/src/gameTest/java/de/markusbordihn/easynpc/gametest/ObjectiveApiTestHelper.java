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
import de.markusbordihn.easynpc.api.event.ObjectiveEventListener;
import de.markusbordihn.easynpc.api.handler.EasyNPCObjectiveHandler;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.CustomMeleeAttackGoal;
import java.util.Optional;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.PathfinderMob;
import net.minecraft.world.entity.ai.attributes.AttributeInstance;
import net.minecraft.world.entity.ai.attributes.Attributes;
import net.minecraft.world.entity.ai.goal.Goal;
import net.minecraft.world.entity.ai.goal.WrappedGoal;
import net.minecraft.world.phys.Vec3;

public class ObjectiveApiTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final int EXPLICIT_PRIORITY = 4;
  private static final Vec3 MELEE_ATTACKER_POSITION = new Vec3(1, 0, 1);
  private static final Vec3 MELEE_TARGET_POSITION = new Vec3(2, 0, 1);
  private static final double FAST_ATTACK_SPEED = 4.0D;
  private static final int MELEE_ATTACK_TEST_TICKS = 40;
  private static final int DEFAULT_MELEE_ATTACKS = 2;
  private static final int FAST_MELEE_ATTACKS = 8;

  private ObjectiveApiTestHelper() {}

  public static void assertOwnerObjectiveWithoutOwnerCanBeRemoved(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    GameTestHelpers.assertTrue(
        helper, "A new NPC must not have an owner", !easyNPC.getEasyNPCOwnerData().hasNPCOwner());

    GameTestHelpers.assertTrue(
        helper,
        "An owner objective must be stored even without an owner",
        EasyNPCObjectiveHandler.addObjective(easyNPC, ObjectiveType.FOLLOW_OWNER));
    GameTestHelpers.assertTrue(
        helper,
        "An owner objective without an owner must be removable",
        EasyNPCObjectiveHandler.removeObjective(easyNPC, ObjectiveType.FOLLOW_OWNER));
    GameTestHelpers.assertTrue(
        helper,
        "A removed owner objective must be gone",
        !EasyNPCObjectiveHandler.hasObjective(easyNPC, ObjectiveType.FOLLOW_OWNER));
  }

  public static void assertObjectiveIsAddedAndRemovedOnce(
      GameTestHelper helper, EntityType<?> entityType, ObjectiveType objectiveType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);

    EasyNPCObjectiveHandler.addObjective(easyNPC, objectiveType);
    EasyNPCObjectiveHandler.addObjective(easyNPC, objectiveType);
    GameTestHelpers.assertEquals(
        helper,
        "Adding the objective " + objectiveType + " twice must keep one entry",
        1L,
        EasyNPCObjectiveHandler.getObjectives(easyNPC).stream()
            .filter(objectiveDataEntry -> objectiveDataEntry.getType() == objectiveType)
            .count());

    GameTestHelpers.assertTrue(
        helper,
        "The objective " + objectiveType + " must be removable",
        EasyNPCObjectiveHandler.removeObjective(easyNPC, objectiveType));
    GameTestHelpers.assertTrue(
        helper,
        "Removing the missing objective " + objectiveType + " again must be rejected",
        !EasyNPCObjectiveHandler.removeObjective(easyNPC, objectiveType));
    GameTestHelpers.assertTrue(
        helper,
        "The removed objective " + objectiveType + " must be gone",
        !EasyNPCObjectiveHandler.hasObjective(easyNPC, objectiveType));
  }

  public static void assertExplicitPriorityReachesGoalSelector(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ServerPlayer owner =
        GameTestHelpers.mockServerPlayer(helper, new Vec3(2, 2, 1), "priority-owner-player");
    easyNPC.getEasyNPCOwnerData().setNPCOwnerUUID(owner.getUUID());
    GameTestHelpers.assertTrue(
        helper,
        "Follow owner objective with an explicit priority was not added",
        EasyNPCObjectiveHandler.addObjective(
            easyNPC, new ObjectiveDataEntry(ObjectiveType.FOLLOW_OWNER, EXPLICIT_PRIORITY)));

    ObjectiveDataEntry storedEntry =
        EasyNPCObjectiveHandler.getObjectives(easyNPC).stream()
            .filter(objectiveDataEntry -> objectiveDataEntry.getType() == ObjectiveType.FOLLOW_OWNER)
            .findFirst()
            .orElse(null);
    GameTestHelpers.assertNotNull(helper, "Follow owner objective is not stored", storedEntry);
    Goal goal = storedEntry.getGoal(easyNPC);
    GameTestHelpers.assertNotNull(helper, "Follow owner objective has no goal", goal);

    Optional<WrappedGoal> wrappedGoal =
        easyNPC.getEntityGoalSelector().getAvailableGoals().stream()
            .filter(availableGoal -> availableGoal.getGoal() == goal)
            .findFirst();
    GameTestHelpers.assertTrue(
        helper, "Follow owner goal is not registered", wrappedGoal.isPresent());
    GameTestHelpers.assertEquals(
        helper,
        "Follow owner goal ignores the explicit priority",
        EXPLICIT_PRIORITY,
        wrappedGoal.get().getPriority());
  }

  public static void assertObjectiveEventsFireOncePerChange(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ObjectiveEventCounter objectiveEventCounter =
        new ObjectiveEventCounter(easyNPC, ObjectiveType.FOLLOW_OWNER);
    EasyNPCEventRegistry.registerObjectiveEventListener(objectiveEventCounter);

    try {
      EasyNPCObjectiveHandler.addObjective(easyNPC, ObjectiveType.FOLLOW_OWNER);
      EasyNPCObjectiveHandler.addObjective(easyNPC, ObjectiveType.FOLLOW_OWNER);
      EasyNPCObjectiveHandler.removeObjective(easyNPC, ObjectiveType.FOLLOW_OWNER);
      EasyNPCObjectiveHandler.removeObjective(easyNPC, ObjectiveType.FOLLOW_OWNER);
    } finally {
      EasyNPCEventRegistry.unregisterObjectiveEventListener(objectiveEventCounter);
    }

    GameTestHelpers.assertEquals(
        helper, "An added objective must fire one event", 1, objectiveEventCounter.addedEvents);
    GameTestHelpers.assertEquals(
        helper, "A removed objective must fire one event", 1, objectiveEventCounter.removedEvents);
  }

  public static void assertMeleeAttackUsesAttackSpeed(
      GameTestHelper helper, EntityType<?> entityType) {
    GameTestHelpers.assertEquals(
        helper,
        "Melee attacks without attack speed",
        DEFAULT_MELEE_ATTACKS,
        countMeleeAttacks(helper, entityType, 0.0D));
    GameTestHelpers.assertEquals(
        helper,
        "Melee attacks ignore the attack speed",
        FAST_MELEE_ATTACKS,
        countMeleeAttacks(helper, entityType, FAST_ATTACK_SPEED));
  }

  private static int countMeleeAttacks(
      GameTestHelper helper, EntityType<?> entityType, double attackSpeed) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, MELEE_ATTACKER_POSITION);
    PathfinderMob attacker = easyNPC.getPathfinderMob();
    AttributeInstance attackSpeedAttribute = attacker.getAttribute(Attributes.ATTACK_SPEED);
    GameTestHelpers.assertNotNull(helper, "NPC has no attack speed", attackSpeedAttribute);
    attackSpeedAttribute.setBaseValue(attackSpeed);
    AttributeInstance attackDamageAttribute = attacker.getAttribute(Attributes.ATTACK_DAMAGE);
    GameTestHelpers.assertNotNull(helper, "NPC has no attack damage", attackDamageAttribute);
    attackDamageAttribute.setBaseValue(1.0D);

    Mob target = spawnTargetWithoutHurtCooldown(helper, attacker);
    CustomMeleeAttackGoal<?> meleeAttackGoal = new CustomMeleeAttackGoal<>(easyNPC, 1.0D, false);
    meleeAttackGoal.start();
    int meleeAttacks = 0;
    for (int tick = 0; tick < MELEE_ATTACK_TEST_TICKS; tick++) {
      meleeAttackGoal.tick();
      if (target.getHealth() < target.getMaxHealth()) {
        meleeAttacks++;
        target.discard();
        target = spawnTargetWithoutHurtCooldown(helper, attacker);
      }
    }
    meleeAttackGoal.stop();
    target.discard();
    easyNPC.getEntity().discard();
    return meleeAttacks;
  }

  private static Mob spawnTargetWithoutHurtCooldown(
      GameTestHelper helper, PathfinderMob attacker) {
    Mob target = GameTestHelpers.spawnEntityType(helper, EntityType.ZOMBIE);
    target.setNoAi(true);
    target.setPos(helper.absoluteVec(MELEE_TARGET_POSITION));
    attacker.setTarget(target);
    return target;
  }

  private static class ObjectiveEventCounter implements ObjectiveEventListener {

    private final EasyNPC<?> easyNPC;
    private final ObjectiveType objectiveType;
    private int addedEvents;
    private int removedEvents;

    private ObjectiveEventCounter(EasyNPC<?> easyNPC, ObjectiveType objectiveType) {
      this.easyNPC = easyNPC;
      this.objectiveType = objectiveType;
    }

    @Override
    public void onObjectiveAdded(EasyNPC<?> easyNPC, ObjectiveDataEntry objectiveDataEntry) {
      if (this.matches(easyNPC, objectiveDataEntry)) {
        this.addedEvents++;
      }
    }

    @Override
    public void onObjectiveRemoved(EasyNPC<?> easyNPC, ObjectiveDataEntry objectiveDataEntry) {
      if (this.matches(easyNPC, objectiveDataEntry)) {
        this.removedEvents++;
      }
    }

    private boolean matches(EasyNPC<?> easyNPC, ObjectiveDataEntry objectiveDataEntry) {
      return easyNPC == this.easyNPC && objectiveDataEntry.getType() == this.objectiveType;
    }
  }
}
