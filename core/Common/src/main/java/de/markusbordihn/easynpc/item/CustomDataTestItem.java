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
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.List;
import net.minecraft.ChatFormatting;
import net.minecraft.nbt.CompoundTag;
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
  public static final String QUOTE_TAG = "CustomQuote";
  public static final String NUMBER_VALUE_TAG = "NumberValue";
  public static final String POWER_LEVEL_TAG = "PowerLevel";
  public static final String USAGE_COUNTER_TAG = "UsageCounter";
  public static final String DEFAULT_QUOTE = "This is a test quote to verify custom data!";
  public static final int DEFAULT_NUMBER_VALUE = 42;
  public static final int DEFAULT_POWER_LEVEL = 9001;
  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  public CustomDataTestItem(Item.Properties properties) {
    super(properties);
  }

  @Override
  public ItemStack getDefaultInstance() {
    ItemStack itemStack = new ItemStack(this);
    CompoundTag compoundTag = new CompoundTag();
    compoundTag.putString(QUOTE_TAG, DEFAULT_QUOTE);
    compoundTag.putInt(NUMBER_VALUE_TAG, DEFAULT_NUMBER_VALUE);
    compoundTag.putInt(POWER_LEVEL_TAG, DEFAULT_POWER_LEVEL);
    compoundTag.putInt(USAGE_COUNTER_TAG, 0);
    itemStack.setTag(compoundTag);
    return itemStack;
  }

  @Override
  public InteractionResultHolder<ItemStack> use(Level level, Player player, InteractionHand hand) {
    ItemStack itemStack = player.getItemInHand(hand);

    if (!level.isClientSide && player instanceof ServerPlayer serverPlayer) {
      CompoundTag compoundTag = itemStack.getTag();

      if (compoundTag == null
          || !compoundTag.contains(QUOTE_TAG)
          || !compoundTag.contains(NUMBER_VALUE_TAG)
          || !compoundTag.contains(POWER_LEVEL_TAG)) {
        serverPlayer.sendSystemMessage(
            Component.literal("❌ ERROR: Custom data is missing or incomplete!")
                .withStyle(ChatFormatting.RED));
        log.error(
            "Custom data test item used by {} but data is missing. Tag: {}",
            serverPlayer.getName().getString(),
            compoundTag);
      } else {
        String quote = compoundTag.getString(QUOTE_TAG);
        int numberValue = compoundTag.getInt(NUMBER_VALUE_TAG);
        int powerLevel = compoundTag.getInt(POWER_LEVEL_TAG);
        int usageCounter =
            compoundTag.contains(USAGE_COUNTER_TAG) ? compoundTag.getInt(USAGE_COUNTER_TAG) : 0;

        usageCounter++;
        compoundTag.putInt(USAGE_COUNTER_TAG, usageCounter);
        itemStack.setTag(compoundTag);

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
            Component.literal("Usage Count: " + usageCounter).withStyle(ChatFormatting.GOLD));

        log.info(
            "Custom data test item verified by {}. Quote: {}, Number: {}, Power: {}, Usage: {}",
            serverPlayer.getName().getString(),
            quote,
            numberValue,
            powerLevel,
            usageCounter);
      }

      return InteractionResultHolder.success(itemStack);
    }

    return InteractionResultHolder.pass(itemStack);
  }

  @Override
  public void appendHoverText(
      ItemStack itemStack, Level level, List<Component> tooltipList, TooltipFlag tooltipFlag) {
    tooltipList.add(TextComponent.getTranslatedTextRaw(Constants.TEXT_ITEM_PREFIX + ID));

    CompoundTag tag = itemStack.getTag();
    if (tag != null && tag.contains(QUOTE_TAG)) {
      int usageCount = tag.contains(USAGE_COUNTER_TAG) ? tag.getInt(USAGE_COUNTER_TAG) : 0;

      tooltipList.add(Component.literal(""));
      tooltipList.add(
          Component.literal("Custom Data Present:")
              .withStyle(ChatFormatting.GOLD, ChatFormatting.BOLD));
      tooltipList.add(
          Component.literal("Quote: \"" + tag.getString(QUOTE_TAG) + "\"")
              .withStyle(ChatFormatting.AQUA));
      tooltipList.add(
          Component.literal("Number: " + tag.getInt(NUMBER_VALUE_TAG))
              .withStyle(ChatFormatting.YELLOW));
      tooltipList.add(
          Component.literal("Power: " + tag.getInt(POWER_LEVEL_TAG))
              .withStyle(ChatFormatting.LIGHT_PURPLE));
      tooltipList.add(
          Component.literal("Times Used: " + usageCount).withStyle(ChatFormatting.GOLD));
    } else {
      tooltipList.add(Component.literal(""));
      tooltipList.add(
          Component.literal("⚠ No custom data")
              .withStyle(ChatFormatting.RED, ChatFormatting.ITALIC));
    }
  }
}
