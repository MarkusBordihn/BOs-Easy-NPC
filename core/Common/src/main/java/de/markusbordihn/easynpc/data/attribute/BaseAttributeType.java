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

package de.markusbordihn.easynpc.data.attribute;

import de.markusbordihn.easynpc.utils.TextUtils;
import java.util.Arrays;
import java.util.Locale;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;
import net.minecraft.core.Holder;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.ai.attributes.Attribute;
import net.minecraft.world.entity.ai.attributes.Attributes;

public enum BaseAttributeType {
  ATTACK_DAMAGE(Attributes.ATTACK_DAMAGE),
  ATTACK_KNOCKBACK(Attributes.ATTACK_KNOCKBACK),
  FOLLOW_RANGE(Attributes.FOLLOW_RANGE),
  KNOCKBACK_RESISTANCE(Attributes.KNOCKBACK_RESISTANCE),
  MAX_HEALTH(Attributes.MAX_HEALTH),
  MOVEMENT_SPEED(Attributes.MOVEMENT_SPEED),
  FLYING_SPEED(Attributes.FLYING_SPEED),
  ATTACK_SPEED(Attributes.ATTACK_SPEED),
  ARMOR(Attributes.ARMOR),
  ARMOR_TOUGHNESS(Attributes.ARMOR_TOUGHNESS),
  LUCK(Attributes.LUCK);

  private final Holder<Attribute> attribute;
  private final String attributeName = this.name().toLowerCase(Locale.ROOT);
  private final String tagName = TextUtils.convertToCamelCase(this.name());
  private final ResourceLocation resourceLocation =
      ResourceLocation.withDefaultNamespace("generic." + this.attributeName);

  BaseAttributeType(Holder<Attribute> attribute) {
    this.attribute = attribute;
  }

  public static BaseAttributeType fromResourceLocation(ResourceLocation resourceLocation) {
    return Lookup.BY_RESOURCE_LOCATION.get(resourceLocation);
  }

  public Holder<Attribute> getAttribute() {
    return this.attribute;
  }

  public String getAttributeName() {
    return this.attributeName;
  }

  public String getTagName() {
    return this.tagName;
  }

  public ResourceLocation getResourceLocation() {
    return this.resourceLocation;
  }

  private static final class Lookup {

    private static final Map<ResourceLocation, BaseAttributeType> BY_RESOURCE_LOCATION =
        Arrays.stream(values())
            .collect(Collectors.toMap(BaseAttributeType::getResourceLocation, Function.identity()));

    private Lookup() {}
  }
}
