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
import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.entity.easynpc.data.ActionEventDataCapable;
import de.markusbordihn.easynpc.entity.easynpc.handlers.InteractionHandler;
import de.markusbordihn.easynpc.item.ModItemTags;
import de.markusbordihn.easynpc.utils.ItemUtils;
import java.util.List;
import net.minecraft.gametest.framework.GameTestHelper;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResult;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.Items;
import net.minecraft.world.phys.Vec3;

public class InteractionResultTestHelper {

  private static final Vec3 NPC_POSITION = new Vec3(1, 2, 1);
  private static final Vec3 PLAYER_POSITION = new Vec3(2, 2, 1);

  private InteractionResultTestHelper() {}

  public static void assertInteractionActionConsumesTheClick(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockNPCWithInteractionAction(helper, entityType);
    ServerPlayer serverPlayer =
        mockPlayerHolding(helper, "interaction-food", new ItemStack(Items.BREAD));

    GameTestHelpers.assertEquals(
        helper,
        "An interaction action must consume the click instead of using the held food",
        InteractionResult.CONSUME,
        InteractionHandler.handleMobInteraction(easyNPC, serverPlayer, InteractionHand.MAIN_HAND));
  }

  public static void assertInteractionWithoutActionPasses(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    easyNPC.getEasyNPCActionEventData().clearActionEventSet();
    ServerPlayer serverPlayer =
        mockPlayerHolding(helper, "interaction-empty", new ItemStack(Items.BREAD));

    GameTestHelpers.assertEquals(
        helper,
        "An NPC without an interaction action must leave the click to the held item",
        InteractionResult.PASS,
        InteractionHandler.handleMobInteraction(easyNPC, serverPlayer, InteractionHand.MAIN_HAND));
  }

  public static void assertInteractionWithToolItemPasses(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockNPCWithInteractionAction(helper, entityType);
    ServerPlayer serverPlayer =
        mockPlayerHolding(
            helper, "interaction-preset", new ItemStack(ItemUtils.getEasyNPCPresetEmptyItem()));

    GameTestHelpers.assertEquals(
        helper,
        "An Easy NPC tool item must handle the click itself",
        InteractionResult.PASS,
        InteractionHandler.handleMobInteraction(easyNPC, serverPlayer, InteractionHand.MAIN_HAND));
  }

  public static void assertPassInteractionTagContainsWeapons(GameTestHelper helper) {
    for (Item item : List.of(Items.BOW, Items.CROSSBOW, Items.TRIDENT, Items.SHIELD)) {
      GameTestHelpers.assertTrue(
          helper,
          item + " must be in the pass_interaction item tag",
          new ItemStack(item).is(ModItemTags.PASS_INTERACTION));
    }
    GameTestHelpers.assertTrue(
        helper,
        "Food must not be in the pass_interaction item tag",
        !new ItemStack(Items.BREAD).is(ModItemTags.PASS_INTERACTION));
  }

  public static void assertPassInteractionItemWithoutActionPasses(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ActionEventDataCapable<?> actionEventData = easyNPC.getEasyNPCActionEventData();
    actionEventData.clearActionEventSet();
    ServerPlayer serverPlayer =
        mockPlayerHolding(helper, "interaction-bow-empty", new ItemStack(Items.BOW));

    GameTestHelpers.assertTrue(
        helper,
        "Clearing the actions must clear the synced interaction flag",
        !actionEventData.hasSynchedInteractionActionEvent());
    GameTestHelpers.assertEquals(
        helper,
        "A bow on an NPC without an interaction action must be used as a bow",
        InteractionResult.PASS,
        InteractionHandler.handleMobInteraction(easyNPC, serverPlayer, InteractionHand.MAIN_HAND));
  }

  public static void assertInteractionActionTakesPrecedenceOverPassInteractionItem(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = mockNPCWithInteractionAction(helper, entityType);
    ServerPlayer serverPlayer =
        mockPlayerHolding(helper, "interaction-bow-action", new ItemStack(Items.BOW));

    GameTestHelpers.assertTrue(
        helper,
        "An interaction action must set the synced interaction flag",
        easyNPC.getEasyNPCActionEventData().hasSynchedInteractionActionEvent());
    GameTestHelpers.assertEquals(
        helper,
        "An interaction action must win over a held bow",
        InteractionResult.CONSUME,
        InteractionHandler.handleMobInteraction(easyNPC, serverPlayer, InteractionHand.MAIN_HAND));
  }

  private static EasyNPC<?> mockNPCWithInteractionAction(
      GameTestHelper helper, EntityType<?> entityType) {
    EasyNPC<?> easyNPC = GameTestHelpers.mockEasyNPC(helper, entityType, NPC_POSITION);
    ActionDataSet actionDataSet = new ActionDataSet();
    actionDataSet.add(
        new ActionDataEntry(ActionDataType.NPC_STATE, "increase gametest_interaction 1"));
    ActionEventSet actionEventSet = new ActionEventSet();
    actionEventSet.setActionEvent(ActionEventType.ON_INTERACTION, actionDataSet);
    easyNPC.getEasyNPCActionEventData().setActionEventSet(actionEventSet);
    return easyNPC;
  }

  private static ServerPlayer mockPlayerHolding(
      GameTestHelper helper, String playerName, ItemStack itemStack) {
    ServerPlayer serverPlayer =
        GameTestHelpers.mockSurvivalServerPlayer(helper, PLAYER_POSITION, playerName);
    serverPlayer.setItemInHand(InteractionHand.MAIN_HAND, itemStack);
    return serverPlayer;
  }
}
