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

import java.util.EnumMap;
import java.util.Map;
import net.minecraft.nbt.CompoundTag;

public class EntityAttributes {

  public static final String ENTITY_ATTRIBUTE_TAG = "EntityAttribute";

  private final Map<EntityAttributeType, EntityAttributesInterface> attributeMap =
      new EnumMap<>(EntityAttributeType.class);

  public EntityAttributes() {
    this.setAttribute(EntityAttributeType.COMBAT, new CombatAttributes());
    this.setAttribute(EntityAttributeType.ENVIRONMENTAL, new EnvironmentalAttributes());
    this.setAttribute(EntityAttributeType.INTERACTION, new InteractionAttributes());
    this.setAttribute(EntityAttributeType.MOVEMENT, new MovementAttributes());
  }

  public EntityAttributes(CompoundTag compoundTag) {
    this();
    this.load(compoundTag);
  }

  public void load(CompoundTag compoundTag) {
    if (!compoundTag.contains(ENTITY_ATTRIBUTE_TAG)
        || compoundTag.getCompound(ENTITY_ATTRIBUTE_TAG).isEmpty()) {
      return;
    }
    CompoundTag entityAttributeTag = compoundTag.getCompound(ENTITY_ATTRIBUTE_TAG);
    this.setAttribute(EntityAttributeType.COMBAT, CombatAttributes.decode(entityAttributeTag));
    this.setAttribute(
        EntityAttributeType.ENVIRONMENTAL, EnvironmentalAttributes.decode(entityAttributeTag));
    this.setAttribute(
        EntityAttributeType.INTERACTION, InteractionAttributes.decode(entityAttributeTag));
    this.setAttribute(EntityAttributeType.MOVEMENT, MovementAttributes.decode(entityAttributeTag));
  }

  public boolean hasAttribute(EntityAttributeType entityAttributeType) {
    return attributeMap.containsKey(entityAttributeType);
  }

  public EntityAttributesInterface getAttribute(EntityAttributeType entityAttributeType) {
    return attributeMap.get(entityAttributeType);
  }

  public void setAttribute(
      EntityAttributeType entityAttributeType, EntityAttributesInterface attribute) {
    attributeMap.put(entityAttributeType, attribute);
  }

  public boolean hasCombatAttributes() {
    return this.hasAttribute(EntityAttributeType.COMBAT);
  }

  public CombatAttributes getCombatAttributes() {
    return (CombatAttributes) this.getAttribute(EntityAttributeType.COMBAT);
  }

  public void setCombatAttributes(CombatAttributes combatAttributes) {
    this.setAttribute(EntityAttributeType.COMBAT, combatAttributes);
  }

  public boolean hasEnvironmentalAttributes() {
    return this.hasAttribute(EntityAttributeType.ENVIRONMENTAL);
  }

  public EnvironmentalAttributes getEnvironmentalAttributes() {
    return (EnvironmentalAttributes) this.getAttribute(EntityAttributeType.ENVIRONMENTAL);
  }

  public void setEnvironmentalAttributes(EnvironmentalAttributes environmentalAttributes) {
    this.setAttribute(EntityAttributeType.ENVIRONMENTAL, environmentalAttributes);
  }

  public boolean hasInteractionAttributes() {
    return this.hasAttribute(EntityAttributeType.INTERACTION);
  }

  public InteractionAttributes getInteractionAttributes() {
    return (InteractionAttributes) this.getAttribute(EntityAttributeType.INTERACTION);
  }

  public void setInteractionAttributes(InteractionAttributes interactionAttributes) {
    this.setAttribute(EntityAttributeType.INTERACTION, interactionAttributes);
  }

  public boolean hasMovementAttributes() {
    return this.hasAttribute(EntityAttributeType.MOVEMENT);
  }

  public MovementAttributes getMovementAttributes() {
    return (MovementAttributes) this.getAttribute(EntityAttributeType.MOVEMENT);
  }

  public void setMovementAttributes(MovementAttributes movementAttributes) {
    this.setAttribute(EntityAttributeType.MOVEMENT, movementAttributes);
  }

  public CompoundTag save(CompoundTag compoundTag) {
    CompoundTag entityAttributeTag = new CompoundTag();
    attributeMap.forEach(
        (type, attribute) -> {
          if (attribute instanceof CombatAttributes combatAttributes) {
            combatAttributes.encode(entityAttributeTag);
          } else if (attribute instanceof EnvironmentalAttributes environmentalAttributes) {
            environmentalAttributes.encode(entityAttributeTag);
          } else if (attribute instanceof InteractionAttributes interactionAttributes) {
            interactionAttributes.encode(entityAttributeTag);
          } else if (attribute instanceof MovementAttributes movementAttributes) {
            movementAttributes.encode(entityAttributeTag);
          }
        });
    compoundTag.put(ENTITY_ATTRIBUTE_TAG, entityAttributeTag);
    return compoundTag;
  }

  public CompoundTag createTag() {
    return this.save(new CompoundTag());
  }
}
