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
import de.markusbordihn.easynpc.component.DataComponents;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.Locale;
import java.util.function.Consumer;
import net.minecraft.ChatFormatting;
import net.minecraft.core.registries.Registries;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextColor;
import net.minecraft.resources.Identifier;
import net.minecraft.resources.ResourceKey;
import net.minecraft.world.item.Item;
import net.minecraft.world.item.ItemStack;
import net.minecraft.world.item.TooltipFlag;
import net.minecraft.world.item.component.CustomData;
import net.minecraft.world.item.component.TooltipDisplay;

public class ConsumableItem extends Item {

  public static final String CUSTOM_NAME_TAG = "Name";
  public static final String CUSTOM_NAME_COLOR_TAG = "Color";
  public static final String CUSTOM_DESCRIPTION_TAG = "Description";

  private final String id;

  public ConsumableItem(String id, Properties properties) {
    super(
        properties.setId(
            ResourceKey.create(
                Registries.ITEM, Identifier.fromNamespaceAndPath(Constants.MOD_ID, id))));
    this.id = id;
  }

  public static ItemStack setCustomName(ItemStack itemStack, String name, ChatFormatting color) {
    CompoundTag compoundTag = getCustomDataOrEmpty(itemStack);
    if (name == null || name.isBlank()) {
      compoundTag.remove(CUSTOM_NAME_TAG);
    } else {
      compoundTag.putString(CUSTOM_NAME_TAG, name);
    }

    if (color == null || TextColor.fromLegacyFormat(color) == null) {
      compoundTag.remove(CUSTOM_NAME_COLOR_TAG);
    } else {
      compoundTag.putString(CUSTOM_NAME_COLOR_TAG, color.name().toLowerCase(Locale.ROOT));
    }

    itemStack.set(DataComponents.CUSTOM_DATA, CustomData.of(compoundTag));
    itemStack.remove(net.minecraft.core.component.DataComponents.CUSTOM_DATA);
    return itemStack;
  }

  private static CompoundTag getCustomDataOrEmpty(ItemStack itemStack) {
    CompoundTag compoundTag = getCustomData(itemStack);
    return compoundTag != null ? compoundTag : new CompoundTag();
  }

  private static CompoundTag getCustomData(ItemStack itemStack) {
    CustomData customData = itemStack.get(DataComponents.CUSTOM_DATA);
    if (customData == null) {
      customData = itemStack.get(net.minecraft.core.component.DataComponents.CUSTOM_DATA);
    }
    return customData != null ? customData.copyTag() : null;
  }

  private static ChatFormatting getCustomNameColor(CompoundTag compoundTag) {
    if (!compoundTag.contains(CUSTOM_NAME_COLOR_TAG)) {
      return null;
    }

    ChatFormatting color =
        parseChatFormatting(compoundTag.getString(CUSTOM_NAME_COLOR_TAG).orElse(""));
    return color != null && TextColor.fromLegacyFormat(color) != null ? color : null;
  }

  private static ChatFormatting parseChatFormatting(String colorName) {
    if (colorName == null || colorName.isBlank()) {
      return null;
    }
    try {
      return ChatFormatting.valueOf(colorName.toUpperCase(Locale.ROOT));
    } catch (IllegalArgumentException e) {
      return null;
    }
  }

  @Override
  public Component getName(ItemStack itemStack) {
    CompoundTag compoundTag = getCustomData(itemStack);
    if (compoundTag == null) {
      return super.getName(itemStack);
    }

    ChatFormatting color = getCustomNameColor(compoundTag);
    String customName = compoundTag.getString(CUSTOM_NAME_TAG).orElse("");
    if (!customName.isBlank()) {
      return color != null
          ? Component.literal(customName).withStyle(color)
          : Component.literal(customName);
    }

    Component name = super.getName(itemStack);
    return color != null ? name.copy().withStyle(color) : name;
  }

  @Override
  public void appendHoverText(
      ItemStack itemStack,
      TooltipContext tooltipContext,
      TooltipDisplay tooltipDisplay,
      Consumer<Component> consumer,
      TooltipFlag tooltipFlag) {
    CompoundTag compoundTag = getCustomData(itemStack);
    if (compoundTag != null) {
      String customDescription = compoundTag.getString(CUSTOM_DESCRIPTION_TAG).orElse("");
      if (!customDescription.isBlank()) {
        consumer.accept(Component.literal(customDescription).withStyle(ChatFormatting.GRAY));
        return;
      }
    }

    consumer.accept(
        TextComponent.getTranslatedTextRaw(Constants.TEXT_ITEM_PREFIX + this.id)
            .withStyle(ChatFormatting.GRAY));
  }
}
