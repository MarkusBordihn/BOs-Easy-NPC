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

package de.markusbordihn.easynpc.data.objective.factory;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertInstanceOf;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import de.markusbordihn.easynpc.data.objective.ObjectiveGoalFactory;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import net.minecraft.SharedConstants;
import net.minecraft.server.Bootstrap;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.animal.Animal;
import net.minecraft.world.entity.monster.Creeper;
import net.minecraft.world.entity.monster.Monster;
import net.minecraft.world.entity.npc.villager.AbstractVillager;
import net.minecraft.world.entity.player.Player;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

class ObjectiveFamilyFactoryTest {

  @BeforeAll
  static void bootstrap() {
    SharedConstants.tryDetectVersion();
    Bootstrap.bootStrap();
  }

  private static ObjectiveGoalFactory unwrap(ObjectiveType objectiveType) {
    ObjectiveGoalFactory objectiveGoalFactory = BuiltInObjectiveFactories.get(objectiveType);
    assertNotNull(objectiveGoalFactory, objectiveType + " must have a registered factory");

    return objectiveGoalFactory instanceof PathfinderMobObjectiveFactory pathfinderMobFactory
        ? pathfinderMobFactory.delegate()
        : objectiveGoalFactory;
  }

  private static void assertAvoidedEntity(
      ObjectiveType objectiveType, Class<? extends LivingEntity> avoidedEntityClass) {
    AvoidEntityGoalFactory avoidEntityGoalFactory =
        assertInstanceOf(AvoidEntityGoalFactory.class, unwrap(objectiveType));
    assertEquals(
        avoidedEntityClass,
        avoidEntityGoalFactory.avoidedEntityClass(),
        objectiveType + " must avoid " + avoidedEntityClass.getSimpleName());
  }

  private static void assertLookAtEntity(
      ObjectiveType objectiveType, Class<? extends LivingEntity> lookAtEntityClass) {
    LookAtEntityGoalFactory lookAtEntityGoalFactory =
        assertInstanceOf(LookAtEntityGoalFactory.class, unwrap(objectiveType));
    assertEquals(
        lookAtEntityClass,
        lookAtEntityGoalFactory.lookAtEntityClass(),
        objectiveType + " must look at " + lookAtEntityClass.getSimpleName());
  }

  private static NearestAttackableTargetFactory attackTargetFactory(ObjectiveType objectiveType) {
    return assertInstanceOf(NearestAttackableTargetFactory.class, unwrap(objectiveType));
  }

  private static void assertAttackedEntity(
      ObjectiveType objectiveType, Class<? extends LivingEntity> targetEntityClass) {
    assertEquals(
        targetEntityClass,
        attackTargetFactory(objectiveType).targetEntityClass(),
        objectiveType + " must target " + targetEntityClass.getSimpleName());
  }

  @Test
  @DisplayName("Each flee objective avoids its own entity type")
  void testFleeObjectivesAvoidTheirEntityType() {
    assertAvoidedEntity(ObjectiveType.FLEE_CREEPER, Creeper.class);
    assertAvoidedEntity(ObjectiveType.FLEE_MOB, Mob.class);
    assertAvoidedEntity(ObjectiveType.FLEE_MONSTER, Monster.class);
    assertAvoidedEntity(ObjectiveType.FLEE_PLAYER, Player.class);
    assertAvoidedEntity(ObjectiveType.FLEE_VILLAGER, AbstractVillager.class);
  }

  @Test
  @DisplayName("Each look objective watches its own entity type")
  void testLookObjectivesWatchTheirEntityType() {
    assertLookAtEntity(ObjectiveType.LOOK_AT_PLAYER, Player.class);
    assertLookAtEntity(ObjectiveType.LOOK_AT_MOB, Mob.class);
    assertLookAtEntity(ObjectiveType.LOOK_AT_ANIMAL, Animal.class);
  }

  @Test
  @DisplayName("Each attack target objective targets its own entity type")
  void testAttackTargetObjectivesTargetTheirEntityType() {
    assertAttackedEntity(ObjectiveType.ATTACK_ANIMAL, Animal.class);
    assertAttackedEntity(ObjectiveType.ATTACK_PLAYER, Player.class);
    assertAttackedEntity(ObjectiveType.ATTACK_MONSTER, Monster.class);
    assertAttackedEntity(ObjectiveType.ATTACK_VILLAGER, AbstractVillager.class);
    assertAttackedEntity(ObjectiveType.ATTACK_MOB, Mob.class);
    assertAttackedEntity(ObjectiveType.ATTACK_MOB_WITHOUT_CREEPER, Mob.class);
    assertAttackedEntity(ObjectiveType.ATTACK_PLAYER_WITHOUT_OWNER, Player.class);
    assertAttackedEntity(ObjectiveType.ATTACK_PLAYER_BY_NAME, Player.class);
    assertAttackedEntity(ObjectiveType.ATTACK_ENTITY_BY_TAG, LivingEntity.class);
    assertAttackedEntity(ObjectiveType.ATTACK_ENTITY_BY_TEAM, LivingEntity.class);
    assertAttackedEntity(ObjectiveType.ATTACK_ENTITY_BY_UUID, LivingEntity.class);
    assertAttackedEntity(ObjectiveType.ATTACK_HOSTILE_FACTIONS, LivingEntity.class);
  }

  @Test
  @DisplayName("Attack targets without a filter keep the plain nearest target behavior")
  void testUnfilteredAttackTargetsHaveNoPredicate() {
    assertNull(attackTargetFactory(ObjectiveType.ATTACK_ANIMAL).predicateFactory());
    assertNull(attackTargetFactory(ObjectiveType.ATTACK_PLAYER).predicateFactory());
    assertNull(attackTargetFactory(ObjectiveType.ATTACK_MONSTER).predicateFactory());
    assertNull(attackTargetFactory(ObjectiveType.ATTACK_VILLAGER).predicateFactory());
    assertNotNull(attackTargetFactory(ObjectiveType.ATTACK_MOB).predicateFactory());
  }
}
