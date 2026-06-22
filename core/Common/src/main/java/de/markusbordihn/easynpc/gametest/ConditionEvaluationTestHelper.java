/*
 * Copyright 2025 Markus Bordihn
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

import de.markusbordihn.easynpc.condition.ConditionManager;
import de.markusbordihn.easynpc.condition.HasItemInHandCondition;
import de.markusbordihn.easynpc.condition.HasItemInInventoryCondition;
import de.markusbordihn.easynpc.condition.HealthConditionEvaluator;
import de.markusbordihn.easynpc.data.action.ActionDataSet;
import de.markusbordihn.easynpc.data.condition.ConditionDataEntry;
import de.markusbordihn.easynpc.data.condition.ConditionOperationType;
import de.markusbordihn.easynpc.data.condition.ConditionType;
import de.markusbordihn.easynpc.data.condition.DurationType;
import de.markusbordihn.easynpc.data.condition.HandItemType;
import de.markusbordihn.easynpc.data.dialog.DialogButtonEntry;
import de.markusbordihn.easynpc.data.dialog.DialogButtonType;
import de.markusbordihn.easynpc.data.dialog.DialogDataEntry;
import de.markusbordihn.easynpc.data.dialog.DialogDataSet;
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.level.GameType;
import net.minecraft.world.phys.Vec3;

public class ConditionEvaluationTestHelper {

  private ConditionEvaluationTestHelper() {}

  public static void assertItemQuantityConditions(GameTestHelper helper) {
    Player player = helper.makeMockPlayer(GameType.SURVIVAL);
    player.getInventory().clearContent();

    ConditionDataEntry needTenDiamonds =
        new ConditionDataEntry(
            ConditionType.HAS_ITEM_IN_INVENTORY,
            ConditionOperationType.EQUALS,
            "minecraft:diamond",
            10);

    player.getInventory().add(new ItemStack(Items.DIAMOND, 9));
    helper.assertTrue(
        !HasItemInInventoryCondition.evaluate(needTenDiamonds, player),
        "9 diamonds should not satisfy a >= 10 condition");

    player.getInventory().add(new ItemStack(Items.DIAMOND, 1));
    helper.assertTrue(
        HasItemInInventoryCondition.evaluate(needTenDiamonds, player),
        "10 diamonds should satisfy a >= 10 condition");

    ConditionDataEntry needOneEmerald =
        new ConditionDataEntry(ConditionType.HAS_ITEM_IN_INVENTORY).withName("minecraft:emerald");
    helper.assertTrue(
        !HasItemInInventoryCondition.evaluate(needOneEmerald, player),
        "no emerald should fail a default (>= 1) condition");
    player.getInventory().add(new ItemStack(Items.EMERALD, 1));
    helper.assertTrue(
        HasItemInInventoryCondition.evaluate(needOneEmerald, player),
        "one emerald should satisfy a default (>= 1) condition");

    ConditionDataEntry notTenGold =
        new ConditionDataEntry(
            ConditionType.HAS_ITEM_IN_INVENTORY,
            ConditionOperationType.NOT_EQUALS,
            "minecraft:gold_ingot",
            10);
    helper.assertTrue(
        HasItemInInventoryCondition.evaluate(notTenGold, player),
        "no gold should satisfy a NOT (>= 10) condition");
    player.getInventory().add(new ItemStack(Items.GOLD_INGOT, 10));
    helper.assertTrue(
        !HasItemInInventoryCondition.evaluate(notTenGold, player),
        "10 gold should fail a NOT (>= 10) condition");

    ConditionDataEntry needFiveSticks =
        new ConditionDataEntry(
            ConditionType.HAS_ITEM_IN_HAND,
            HandItemType.MAIN_HAND,
            ConditionOperationType.EQUALS,
            "minecraft:stick",
            5);
    player.setItemInHand(InteractionHand.MAIN_HAND, new ItemStack(Items.STICK, 4));
    helper.assertTrue(
        !HasItemInHandCondition.evaluate(needFiveSticks, player),
        "4 sticks in hand should not satisfy a >= 5 condition");
    player.setItemInHand(InteractionHand.MAIN_HAND, new ItemStack(Items.STICK, 5));
    helper.assertTrue(
        HasItemInHandCondition.evaluate(needFiveSticks, player),
        "5 sticks in hand should satisfy a >= 5 condition");
  }

  public static void assertButtonExecutionLimitEnforced(GameTestHelper helper) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));

    int limit = 3;
    ConditionDataEntry executionLimit =
        new ConditionDataEntry(
            ConditionType.EXECUTION_LIMIT,
            DurationType.PER_DAY,
            ConditionOperationType.NONE,
            "",
            limit);
    Set<ConditionDataEntry> conditions = new LinkedHashSet<>();
    conditions.add(executionLimit);

    DialogButtonEntry button =
        new DialogButtonEntry(
            "limited_button",
            "limited_button",
            DialogButtonType.DEFAULT,
            new ActionDataSet(),
            conditions);

    // Mirror ExecuteDialogButtonActionMessage: check and record both keyed by the button id.
    for (int click = 1; click <= limit; click++) {
      GameTestHelpers.assertTrue(
          helper,
          "Click " + click + " of " + limit + " should be allowed",
          ConditionManager.evaluateAll(button.conditions(), serverPlayer, button.id()));
      for (ConditionDataEntry condition : button.conditions()) {
        ConditionManager.recordExecution(condition, serverPlayer, button.id());
      }
    }

    GameTestHelpers.assertTrue(
        helper,
        "Click " + (limit + 1) + " should be blocked once the daily limit is reached",
        !ConditionManager.evaluateAll(button.conditions(), serverPlayer, button.id()));

    DialogButtonEntry otherButton =
        new DialogButtonEntry(
            "other_button",
            "other_button",
            DialogButtonType.DEFAULT,
            new ActionDataSet(),
            conditions);
    GameTestHelpers.assertTrue(
        helper,
        "A different button id must not share the execution count",
        ConditionManager.evaluateAll(otherButton.conditions(), serverPlayer, otherButton.id()));
  }

  public static void assertConditionalDialogOpenRespectsConditions(GameTestHelper helper) {
    ServerPlayer serverPlayer = GameTestHelpers.mockServerPlayer(helper, new Vec3(1, 2, 1));
    serverPlayer.getInventory().clearContent();
    serverPlayer.setItemInHand(InteractionHand.MAIN_HAND, ItemStack.EMPTY);

    ConditionDataEntry needStick =
        new ConditionDataEntry(
            ConditionType.HAS_ITEM_IN_HAND,
            HandItemType.MAIN_HAND,
            ConditionOperationType.EQUALS,
            "minecraft:stick",
            1);
    Set<ConditionDataEntry> conditions = new LinkedHashSet<>();
    conditions.add(needStick);

    DialogDataEntry dialog = new DialogDataEntry("gated_dialog", "Gated", "Hello");
    dialog.setConditions(conditions);

    DialogDataSet dialogDataSet = new DialogDataSet();
    dialogDataSet.addDialog(dialog);
    UUID dialogId = dialog.getId();

    // The unconditional open path (getDialog) always resolves the dialog.
    GameTestHelpers.assertTrue(
        helper,
        "Dialog must exist regardless of conditions",
        dialogDataSet.getDialog(dialogId) != null);

    // The conditional open path is blocked while the condition is not met.
    GameTestHelpers.assertTrue(
        helper,
        "Conditional open must be blocked when the dialog condition is not met",
        !dialogDataSet.canOpenDialog(dialogId, serverPlayer));

    // The conditional open path is allowed once the condition is met.
    serverPlayer.setItemInHand(InteractionHand.MAIN_HAND, new ItemStack(Items.STICK, 1));
    GameTestHelpers.assertTrue(
        helper,
        "Conditional open must be allowed when the dialog condition is met",
        dialogDataSet.canOpenDialog(dialogId, serverPlayer));
  }

  public static void assertHealthTargetConditions(GameTestHelper helper, EntityType<?> entityType) {
    // The NPC context provides both the NPC_HEALTH target and the level used for UUID lookups.
    EasyNPC<?> npc = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(2, 2, 1));
    LivingEntity npcEntity = npc.getLivingEntity();

    npcEntity.setHealth(npcEntity.getMaxHealth());
    GameTestHelpers.assertTrue(
        helper,
        "Full NPC health must not satisfy a < 75% condition",
        !HealthConditionEvaluator.evaluate(ConditionOperationType.LESS_THAN, 75, npcEntity));

    npcEntity.setHealth(npcEntity.getMaxHealth() * 0.5F);
    GameTestHelpers.assertTrue(
        helper,
        "Half NPC health must satisfy a < 75% condition",
        HealthConditionEvaluator.evaluate(ConditionOperationType.LESS_THAN, 75, npcEntity));

    // ENTITY_HEALTH targets a second entity by UUID, resolved from the NPC context level.
    EasyNPC<?> target = GameTestHelpers.mockEasyNPC(helper, entityType, new Vec3(3, 2, 1));
    LivingEntity targetEntity = target.getLivingEntity();
    String targetUuid = targetEntity.getUUID().toString();

    GameTestHelpers.assertTrue(
        helper,
        "Target entity must be resolvable by UUID from the NPC context level",
        HealthConditionEvaluator.resolveByUuid(npcEntity, targetUuid) == targetEntity);

    targetEntity.setHealth(targetEntity.getMaxHealth());
    GameTestHelpers.assertTrue(
        helper,
        "Full target health must not satisfy a < 75% condition",
        !HealthConditionEvaluator.evaluate(
            ConditionOperationType.LESS_THAN,
            75,
            HealthConditionEvaluator.resolveByUuid(npcEntity, targetUuid)));

    targetEntity.setHealth(targetEntity.getMaxHealth() * 0.5F);
    GameTestHelpers.assertTrue(
        helper,
        "Half target health must satisfy a < 75% condition",
        HealthConditionEvaluator.evaluate(
            ConditionOperationType.LESS_THAN,
            75,
            HealthConditionEvaluator.resolveByUuid(npcEntity, targetUuid)));

    // An unknown UUID must resolve to null and therefore evaluate to false.
    LivingEntity unknownTarget =
        HealthConditionEvaluator.resolveByUuid(npcEntity, UUID.randomUUID().toString());
    GameTestHelpers.assertTrue(
        helper, "Unknown entity UUID must resolve to null", unknownTarget == null);
    GameTestHelpers.assertTrue(
        helper,
        "Unknown entity UUID must evaluate to false",
        !HealthConditionEvaluator.evaluate(ConditionOperationType.LESS_THAN, 75, unknownTarget));
  }
}
