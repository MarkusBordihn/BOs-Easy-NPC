/*
 * Copyright 2023 Markus Bordihn
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

package de.markusbordihn.easynpc.item;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.data.test.TestItemData;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.List;
import net.minecraft.ChatFormatting;
import net.minecraft.network.chat.Component;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.InteractionHand;
import net.minecraft.world.InteractionResultHolder;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.level.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class CustomDataTestItem extends Item {

  public static final String ID = "custom_data_test";
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public CustomDataTestItem(Item.Properties properties) {
    super(properties);
  }

  @Override
  public ItemStack getDefaultInstance() {
    ItemStack itemStack = new ItemStack(this);
    itemStack.set(
        de.markusbordihn.easynpc.component.DataComponents.TEST_ITEM_DATA, TestItemData.DEFAULT);
    return itemStack;
  }

  @Override
  public InteractionResultHolder<ItemStack> use(Level level, Player player, InteractionHand hand) {
    ItemStack itemStack = player.getItemInHand(hand);

    if (!level.isClientSide && player instanceof ServerPlayer serverPlayer) {
      TestItemData testItemData = TestItemData.get(itemStack);

      if (testItemData == null) {
        serverPlayer.sendSystemMessage(
            Component.literal("❌ ERROR: Custom data is missing or incomplete!")
                .withStyle(ChatFormatting.RED));
        log.error(
            "Custom data test item used by {} but data is missing.",
            serverPlayer.getName().getString());
      } else {
        String quote = testItemData.quote();
        int numberValue = testItemData.numberValue();
        int powerLevel = testItemData.powerLevel();
        int usageCounter = testItemData.usageCounter();

        // Increment usage counter and update the item
        TestItemData updatedData = testItemData.withIncrementedUsageCounter();
        itemStack.set(
            de.markusbordihn.easynpc.component.DataComponents.TEST_ITEM_DATA, updatedData);

        serverPlayer.sendSystemMessage(
            Component.literal("✓ Custom data verified successfully!")
                .withStyle(ChatFormatting.GREEN));
        serverPlayer.sendSystemMessage(
            Component.literal("Quote: \"" + quote + "\"").withStyle(ChatFormatting.AQUA));
        serverPlayer.sendSystemMessage(
            Component.literal("Number Value: " + numberValue).withStyle(ChatFormatting.YELLOW));
        serverPlayer.sendSystemMessage(
            Component.literal("Power Level: " + powerLevel).withStyle(ChatFormatting.LIGHT_PURPLE));
        serverPlayer.sendSystemMessage(
            Component.literal("Usage Count: " + (usageCounter + 1)).withStyle(ChatFormatting.GOLD));

        log.info(
            "Custom data test item verified by {}. Quote: {}, Number: {}, Power: {}, Usage: {}",
            serverPlayer.getName().getString(),
            quote,
            numberValue,
            powerLevel,
            usageCounter + 1);
      }

      return InteractionResultHolder.success(itemStack);
    }

    return InteractionResultHolder.pass(itemStack);
  }

  @Override
  public void appendHoverText(
      ItemStack itemStack,
      TooltipContext tooltipContext,
      List<Component> tooltipList,
      TooltipFlag tooltipFlag) {
    tooltipList.add(TextComponent.getTranslatedTextRaw(Constants.TEXT_ITEM_PREFIX + ID));

    TestItemData testItemData = TestItemData.get(itemStack);
    if (testItemData != null) {
      tooltipList.add(Component.literal(""));
      tooltipList.add(
          Component.literal("Custom Data Present:")
              .withStyle(ChatFormatting.GOLD, ChatFormatting.BOLD));
      tooltipList.add(
          Component.literal("Quote: \"" + testItemData.quote() + "\"")
              .withStyle(ChatFormatting.AQUA));
      tooltipList.add(
          Component.literal("Number: " + testItemData.numberValue())
              .withStyle(ChatFormatting.YELLOW));
      tooltipList.add(
          Component.literal("Power: " + testItemData.powerLevel())
              .withStyle(ChatFormatting.LIGHT_PURPLE));
      tooltipList.add(
          Component.literal("Times Used: " + testItemData.usageCounter())
              .withStyle(ChatFormatting.GOLD));
    } else {
      tooltipList.add(Component.literal(""));
      tooltipList.add(
          Component.literal("⚠ No custom data")
              .withStyle(ChatFormatting.RED, ChatFormatting.ITALIC));
    }
  }
}
