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

import static de.markusbordihn.easynpc.data.objective.ObjectiveType.BOW_ATTACK;
import static de.markusbordihn.easynpc.data.objective.ObjectiveType.CROSSBOW_ATTACK;
import static de.markusbordihn.easynpc.data.objective.ObjectiveType.MELEE_ATTACK;
import static de.markusbordihn.easynpc.data.objective.ObjectiveType.PROJECTILE_ATTACK;
import static de.markusbordihn.easynpc.data.objective.ObjectiveType.SMART_ATTACK;
import static de.markusbordihn.easynpc.data.objective.ObjectiveType.ZOMBIE_ATTACK;
import static net.minecraft.world.entity.EntityTypes.BEE;
import static net.minecraft.world.entity.EntityTypes.BLAZE;
import static net.minecraft.world.entity.EntityTypes.CREEPER;
import static net.minecraft.world.entity.EntityTypes.PHANTOM;
import static net.minecraft.world.entity.EntityTypes.VEX;
import static net.minecraft.world.entity.EntityTypes.ZOMBIE;
import static net.minecraft.world.item.Items.AIR;
import static net.minecraft.world.item.Items.BOW;
import static net.minecraft.world.item.Items.CROSSBOW;
import static net.minecraft.world.item.Items.IRON_AXE;
import static net.minecraft.world.item.Items.IRON_SWORD;
import static net.minecraft.world.item.Items.LANTERN;
import static net.minecraft.world.item.Items.TRIDENT;

import de.markusbordihn.easynpc.api.handler.EasyNPCObjectiveHandler;
import de.markusbordihn.easynpc.data.objective.ObjectiveDataEntry;
import de.markusbordihn.easynpc.data.objective.ObjectiveGroup;
import de.markusbordihn.easynpc.data.objective.ObjectiveType;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.ai.goal.SmartAttackGoal;
import de.markusbordihn.easynpc.entity.easynpc.handlers.SmartAttackHandler;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.world.SimpleContainer;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.Mob;
import net.minecraft.world.entity.ai.goal.WrappedGoal;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.phys.Vec3;

public class SmartAttackTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(2, 2, 2);
  private static final Vec3 TARGET_POSITION = new Vec3(1, 2, 1);
  private static final int PLAYER_SLOT = 0;
  private static final int NPC_OWNED_SLOT = 9;
  private static final List<ObjectiveType> MELEE = List.of(MELEE_ATTACK);
  private static final List<ObjectiveType> MELEE_AND_BOW = List.of(MELEE_ATTACK, BOW_ATTACK);
  private static final List<ObjectiveType> MELEE_AND_CROSSBOW =
      List.of(MELEE_ATTACK, CROSSBOW_ATTACK);
  private static final List<ObjectiveType> MELEE_AND_PROJECTILE =
      List.of(MELEE_ATTACK, PROJECTILE_ATTACK);
  private static final List<ObjectiveType> ZOMBIE_AND_BOW = List.of(ZOMBIE_ATTACK, BOW_ATTACK);
  private static final List<ObjectiveType> BOW_AND_PROJECTILE =
      List.of(BOW_ATTACK, PROJECTILE_ATTACK);
  private static final List<ObjectiveType> BOW_AND_CROSSBOW = List.of(BOW_ATTACK, CROSSBOW_ATTACK);
  private static final List<ObjectiveType> MELEE_BOW_AND_CROSSBOW =
      List.of(MELEE_ATTACK, BOW_ATTACK, CROSSBOW_ATTACK);
  private static final List<SmartAttackCase> SMART_ATTACK_CASES =
      List.of(
          new SmartAttackCase(MELEE_AND_BOW, IRON_SWORD, BOW, ZOMBIE, IRON_SWORD, MELEE_ATTACK),
          new SmartAttackCase(MELEE_AND_BOW, IRON_SWORD, BOW, PHANTOM, BOW, BOW_ATTACK),
          new SmartAttackCase(MELEE_AND_BOW, IRON_SWORD, BOW, CREEPER, BOW, BOW_ATTACK),
          new SmartAttackCase(MELEE_AND_BOW, BOW, IRON_SWORD, ZOMBIE, IRON_SWORD, MELEE_ATTACK),
          new SmartAttackCase(MELEE_AND_BOW, AIR, BOW, ZOMBIE, BOW, BOW_ATTACK),
          new SmartAttackCase(MELEE_AND_BOW, IRON_SWORD, AIR, PHANTOM, IRON_SWORD, MELEE_ATTACK),
          new SmartAttackCase(MELEE_AND_BOW, TRIDENT, BOW, ZOMBIE, TRIDENT, MELEE_ATTACK),
          new SmartAttackCase(
              MELEE_AND_BOW, IRON_SWORD, CROSSBOW, CREEPER, IRON_SWORD, MELEE_ATTACK),
          new SmartAttackCase(
              MELEE_AND_CROSSBOW, IRON_SWORD, CROSSBOW, CREEPER, CROSSBOW, CROSSBOW_ATTACK),
          new SmartAttackCase(
              MELEE_AND_CROSSBOW, IRON_SWORD, CROSSBOW, ZOMBIE, IRON_SWORD, MELEE_ATTACK),
          new SmartAttackCase(ZOMBIE_AND_BOW, IRON_SWORD, BOW, ZOMBIE, IRON_SWORD, MELEE_ATTACK),
          new SmartAttackCase(ZOMBIE_AND_BOW, IRON_SWORD, BOW, BLAZE, BOW, BOW_ATTACK),
          new SmartAttackCase(
              MELEE_AND_PROJECTILE, IRON_SWORD, AIR, ZOMBIE, IRON_SWORD, MELEE_ATTACK),
          new SmartAttackCase(
              MELEE_AND_PROJECTILE, IRON_SWORD, AIR, PHANTOM, IRON_SWORD, PROJECTILE_ATTACK),
          new SmartAttackCase(
              MELEE_AND_PROJECTILE, IRON_SWORD, AIR, BLAZE, IRON_SWORD, PROJECTILE_ATTACK),
          new SmartAttackCase(MELEE_AND_PROJECTILE, AIR, AIR, ZOMBIE, AIR, PROJECTILE_ATTACK),
          new SmartAttackCase(
              MELEE_AND_PROJECTILE, AIR, IRON_SWORD, ZOMBIE, IRON_SWORD, MELEE_ATTACK),
          new SmartAttackCase(
              MELEE_AND_PROJECTILE, IRON_SWORD, BOW, PHANTOM, IRON_SWORD, PROJECTILE_ATTACK),
          new SmartAttackCase(BOW_AND_PROJECTILE, AIR, BOW, ZOMBIE, BOW, BOW_ATTACK),
          new SmartAttackCase(BOW_AND_PROJECTILE, AIR, AIR, ZOMBIE, AIR, PROJECTILE_ATTACK),
          new SmartAttackCase(
              BOW_AND_PROJECTILE, IRON_SWORD, AIR, ZOMBIE, IRON_SWORD, PROJECTILE_ATTACK),
          new SmartAttackCase(BOW_AND_CROSSBOW, AIR, CROSSBOW, VEX, CROSSBOW, CROSSBOW_ATTACK),
          new SmartAttackCase(
              MELEE_BOW_AND_CROSSBOW, IRON_AXE, BOW, ZOMBIE, IRON_AXE, MELEE_ATTACK),
          new SmartAttackCase(MELEE, LANTERN, AIR, ZOMBIE, LANTERN, MELEE_ATTACK),
          new SmartAttackCase(MELEE, LANTERN, IRON_SWORD, ZOMBIE, IRON_SWORD, MELEE_ATTACK));

  private SmartAttackTestHelper() {}

  private static EasyNPC<?> mockNPC(
      GameTestHelper helper,
      EntityType<?> entityType,
      Item mainHandItem,
      List<ObjectiveType> objectiveTypes) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.getLivingEntity().setItemSlot(EquipmentSlot.MAINHAND, new ItemStack(mainHandItem));
    for (ObjectiveType objectiveType : objectiveTypes) {
      GameTestHelpers.assertTrue(
          helper,
          "The " + objectiveType + " objective must be added",
          EasyNPCObjectiveHandler.addObjective(easyNPC, new ObjectiveDataEntry(objectiveType)));
    }
    return easyNPC;
  }

  private static EasyNPC<?> mockSmartAttackNPC(
      GameTestHelper helper,
      EntityType<?> entityType,
      Item mainHandItem,
      Item npcOwnedSlotItem,
      List<ObjectiveType> attackTypes) {
    EasyNPC<?> easyNPC = mockNPC(helper, entityType, mainHandItem, attackTypes);
    GameTestHelpers.assertTrue(
        helper,
        "The smart attack objective must be added",
        EasyNPCObjectiveHandler.addObjective(easyNPC, new ObjectiveDataEntry(SMART_ATTACK)));
    getInventory(easyNPC).setItem(NPC_OWNED_SLOT, new ItemStack(npcOwnedSlotItem));
    return easyNPC;
  }

  private static SimpleContainer getInventory(EasyNPC<?> easyNPC) {
    return easyNPC.getEasyNPCInventoryData().getEasyNPCInventory();
  }

  private static SmartAttackGoal<?> getSmartAttackGoal(GameTestHelper helper, EasyNPC<?> easyNPC) {
    SmartAttackGoal<?> smartAttackGoal =
        easyNPC.getEntityGoalSelector().getAvailableGoals().stream()
            .map(WrappedGoal::getGoal)
            .filter(SmartAttackGoal.class::isInstance)
            .map(SmartAttackGoal.class::cast)
            .findFirst()
            .orElse(null);
    GameTestHelpers.assertNotNull(
        helper, "The smart attack goal must be registered", smartAttackGoal);
    return smartAttackGoal;
  }

  private static Mob fightTarget(
      GameTestHelper helper, EasyNPC<?> easyNPC, EntityType<?> targetType) {
    Mob target = GameTestHelpers.spawnEntityType(helper, targetType);
    target.setPos(helper.absoluteVec(TARGET_POSITION));
    target.setNoAi(true);
    easyNPC.getMob().setTarget(target);
    getSmartAttackGoal(helper, easyNPC).tick();
    return target;
  }

  private static void assertMainHand(GameTestHelper helper, EasyNPC<?> easyNPC, Item item) {
    GameTestHelpers.assertEquals(
        helper, "Main hand item", item, easyNPC.getLivingEntity().getMainHandItem().getItem());
  }

  private static void assertSlot(GameTestHelper helper, EasyNPC<?> easyNPC, int slot, Item item) {
    GameTestHelpers.assertEquals(
        helper, "Item in slot " + slot, item, getInventory(easyNPC).getItem(slot).getItem());
  }

  private static int countItems(EasyNPC<?> easyNPC) {
    SimpleContainer inventory = getInventory(easyNPC);
    int itemCount = easyNPC.getLivingEntity().getMainHandItem().getCount();
    for (int slot = 0; slot < inventory.getContainerSize(); slot++) {
      itemCount += inventory.getItem(slot).getCount();
    }
    return itemCount;
  }

  public static void assertSmartAttackPicksWeaponForTarget(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSmartAttackNPC(helper, entityType, IRON_SWORD, BOW, MELEE_AND_BOW);
    int itemCount = countItems(easyNPC);

    fightTarget(helper, easyNPC, PHANTOM);
    assertMainHand(helper, easyNPC, BOW);
    assertSlot(helper, easyNPC, NPC_OWNED_SLOT, IRON_SWORD);

    fightTarget(helper, easyNPC, ZOMBIE);
    assertMainHand(helper, easyNPC, IRON_SWORD);

    fightTarget(helper, easyNPC, CREEPER);
    assertMainHand(helper, easyNPC, BOW);
    GameTestHelpers.assertEquals(helper, "Item count after swaps", itemCount, countItems(easyNPC));
  }

  public static void assertSmartAttackCombinations(
      GameTestHelper helper, EntityType<?> entityType) {
    for (SmartAttackCase smartAttackCase : SMART_ATTACK_CASES) {
      assertSmartAttackCase(helper, entityType, smartAttackCase);
    }
  }

  private static void assertSmartAttackCase(
      GameTestHelper helper, EntityType<?> entityType, SmartAttackCase smartAttackCase) {
    EasyNPC<?> easyNPC =
        mockSmartAttackNPC(
            helper,
            entityType,
            smartAttackCase.mainHandItem(),
            smartAttackCase.npcOwnedSlotItem(),
            smartAttackCase.attackTypes());
    int itemCount = countItems(easyNPC);

    Mob target = fightTarget(helper, easyNPC, smartAttackCase.targetType());

    GameTestHelpers.assertEquals(
        helper,
        smartAttackCase + ": main hand item",
        smartAttackCase.expectedMainHandItem(),
        easyNPC.getLivingEntity().getMainHandItem().getItem());
    GameTestHelpers.assertEquals(
        helper,
        smartAttackCase + ": allowed attack types",
        Set.of(smartAttackCase.expectedAttackType()),
        ObjectiveGroup.ATTACK_TYPE.stream()
            .filter(attackType -> SmartAttackHandler.isAttackTypeAllowed(easyNPC, attackType))
            .collect(Collectors.toSet()));
    GameTestHelpers.assertEquals(
        helper, smartAttackCase + ": item count", itemCount, countItems(easyNPC));

    target.discard();
    easyNPC.getLivingEntity().discard();
  }

  public static void assertSmartAttackFollowsFlyingAnimal(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSmartAttackNPC(helper, entityType, IRON_SWORD, BOW, MELEE_AND_BOW);
    Mob bee = GameTestHelpers.spawnEntityType(helper, BEE);
    bee.setPos(helper.absoluteVec(TARGET_POSITION));
    bee.setNoAi(true);
    bee.setOnGround(true);
    easyNPC.getMob().setTarget(bee);
    SmartAttackGoal<?> smartAttackGoal = getSmartAttackGoal(helper, easyNPC);

    smartAttackGoal.tick();
    assertMainHand(helper, easyNPC, IRON_SWORD);

    bee.setOnGround(false);
    smartAttackGoal.tick();
    assertMainHand(helper, easyNPC, IRON_SWORD);

    easyNPC.getLivingEntity().tickCount += SmartAttackGoal.WEAPON_SELECTION_INTERVAL_TICKS;
    smartAttackGoal.tick();
    assertMainHand(helper, easyNPC, BOW);
    assertSlot(helper, easyNPC, NPC_OWNED_SLOT, IRON_SWORD);
  }

  public static void assertSmartAttackIgnoresPlayerSlots(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSmartAttackNPC(helper, entityType, AIR, AIR, MELEE_AND_BOW);
    getInventory(easyNPC).setItem(PLAYER_SLOT, new ItemStack(BOW));

    fightTarget(helper, easyNPC, PHANTOM);
    assertMainHand(helper, easyNPC, AIR);
    assertSlot(helper, easyNPC, PLAYER_SLOT, BOW);
  }

  public static void assertSmartAttackRestoresMainHandAfterFight(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockSmartAttackNPC(helper, entityType, LANTERN, IRON_SWORD, MELEE);

    fightTarget(helper, easyNPC, ZOMBIE);
    assertMainHand(helper, easyNPC, IRON_SWORD);

    easyNPC.getMob().setTarget(null);
    SmartAttackGoal<?> smartAttackGoal = getSmartAttackGoal(helper, easyNPC);
    GameTestHelpers.assertTrue(
        helper, "The goal must keep running to restore the item", smartAttackGoal.canUse());
    smartAttackGoal.tick();
    assertMainHand(helper, easyNPC, IRON_SWORD);

    easyNPC.getLivingEntity().tickCount += SmartAttackGoal.RESTORE_MAIN_HAND_DELAY_TICKS;
    smartAttackGoal.tick();
    assertMainHand(helper, easyNPC, LANTERN);
    assertSlot(helper, easyNPC, NPC_OWNED_SLOT, IRON_SWORD);
    GameTestHelpers.assertTrue(
        helper, "The goal must stop after restoring the item", !smartAttackGoal.canUse());
  }

  public static void assertAttackTypesWithoutSmartAttackStayUnchanged(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockNPC(helper, entityType, IRON_SWORD, MELEE_AND_PROJECTILE);
    Mob target = GameTestHelpers.spawnEntityType(helper, PHANTOM);
    target.setNoAi(true);
    easyNPC.getMob().setTarget(target);

    GameTestHelpers.assertTrue(
        helper,
        "Without smart attack every attack type stays allowed",
        SmartAttackHandler.isAttackTypeAllowed(easyNPC, MELEE_ATTACK)
            && SmartAttackHandler.isAttackTypeAllowed(easyNPC, PROJECTILE_ATTACK));
    GameTestHelpers.assertTrue(
        helper,
        "Without smart attack no smart attack goal is registered",
        easyNPC.getEntityGoalSelector().getAvailableGoals().stream()
            .noneMatch(wrappedGoal -> wrappedGoal.getGoal() instanceof SmartAttackGoal));
  }

  public static void assertSmartAttackCommandNeedsAttackType(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.registerEasyNPCDefaultData();
    String setAttackType = "easy_npc objective " + easyNPC.getEntityUUID() + " set attack type ";

    GameTestHelpers.assertEquals(
        helper,
        "Smart attack alone command result",
        0,
        GameTestHelpers.executeCommand(
            helper,
            GameTestHelpers.consoleCommandSource(helper, NPC_POSITION),
            setAttackType + "smart"));
    GameTestHelpers.assertEquals(
        helper,
        "Smart attack with melee and bow command result",
        1,
        GameTestHelpers.executeCommand(
            helper,
            GameTestHelpers.consoleCommandSource(helper, NPC_POSITION),
            setAttackType + "melee bow smart"));
    GameTestHelpers.assertTrue(
        helper,
        "Smart attack must be set",
        EasyNPCObjectiveHandler.hasObjective(easyNPC, SMART_ATTACK));
  }

  private record SmartAttackCase(
      List<ObjectiveType> attackTypes,
      Item mainHandItem,
      Item npcOwnedSlotItem,
      EntityType<?> targetType,
      Item expectedMainHandItem,
      ObjectiveType expectedAttackType) {}
}
