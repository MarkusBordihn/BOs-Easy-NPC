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

package de.markusbordihn.easynpc.compat.easymodelentities;

import de.markusbordihn.easynpc.api.animation.ModelAnimationInfo;
import de.markusbordihn.easynpc.compat.CompatConstants;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.model.ModelType;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import net.minecraft.resources.Identifier;

public final class EasyModelEntitiesManager {

  public static final String INTEGRATION_ID = CompatConstants.MOD_EASY_MODEL_ENTITIES_ID;
  public static final String DEFAULT_PROFILE =
      "easy_model_entities_examples:entity/little_explorer";

  public static final String FLOATING_BODY_TYPE = "FLOATING";
  public static final double DEFAULT_HOVER_HEIGHT = 2.0D;

  private static final Map<Identifier, ModelType> PROFILE_MODEL_TYPES = new ConcurrentHashMap<>();
  private static final Map<Identifier, String> PROFILE_BODY_TYPES = new ConcurrentHashMap<>();
  private static final Map<Identifier, ProfileDimensions> PROFILE_DIMENSIONS =
      new ConcurrentHashMap<>();
  private static final AnimationProvider EMPTY_ANIMATION_PROVIDER =
      new AnimationProvider() {
        @Override
        public List<ModelAnimationInfo> listAnimations(Identifier profileId) {
          return List.of();
        }

        @Override
        public List<String> listAnimationVariants(Identifier profileId, String baseName) {
          return List.of();
        }
      };
  private static final TextureProvider EMPTY_TEXTURE_PROVIDER =
      new TextureProvider() {
        @Override
        public List<String> listTextureSlots(Identifier profileId) {
          return List.of();
        }

        @Override
        public List<Identifier> listTextureVariants(Identifier profileId, String slot) {
          return List.of();
        }
      };

  private static volatile AnimationProvider animationProvider = EMPTY_ANIMATION_PROVIDER;
  private static volatile TextureProvider textureProvider = EMPTY_TEXTURE_PROVIDER;

  private EasyModelEntitiesManager() {}

  public static ProfileDimensions getProfileDimensions(Identifier profileId) {
    return PROFILE_DIMENSIONS.get(profileId);
  }

  public static void registerProfileModelType(Identifier profileId, ModelType modelType) {
    if (profileId != null && modelType != null) {
      PROFILE_MODEL_TYPES.put(profileId, modelType);
    }
  }

  public static void registerProfileBodyType(Identifier profileId, String bodyTypeName) {
    if (profileId != null && bodyTypeName != null && !bodyTypeName.isEmpty()) {
      PROFILE_BODY_TYPES.put(profileId, bodyTypeName.toUpperCase(Locale.ROOT));
    }
  }

  public static void registerProfileDimensions(
      Identifier profileId, float width, float height, float eyeHeight) {
    if (profileId != null && width > 0.0F && height > 0.0F) {
      PROFILE_DIMENSIONS.put(profileId, new ProfileDimensions(width, height, eyeHeight));
    }
  }

  public static void clearProfileModelTypes() {
    clearProfileModelMetadata();
    PROFILE_DIMENSIONS.clear();
  }

  // The client reload only knows the renderable profiles, so the server side dimensions of a
  // profile without a render profile have to survive it.
  public static void clearProfileModelMetadata() {
    PROFILE_MODEL_TYPES.clear();
    PROFILE_BODY_TYPES.clear();
  }

  public static ModelType getProfileModelType(Identifier profileId) {
    return PROFILE_MODEL_TYPES.getOrDefault(profileId, ModelType.HUMANOID);
  }

  public static String getProfileBodyType(Identifier profileId) {
    return PROFILE_BODY_TYPES.get(profileId);
  }

  public static boolean isFloatingProfile(Identifier profileId) {
    return FLOATING_BODY_TYPE.equals(PROFILE_BODY_TYPES.get(profileId));
  }

  public static void setAnimationProvider(AnimationProvider provider) {
    animationProvider = provider != null ? provider : EMPTY_ANIMATION_PROVIDER;
  }

  public static List<ModelAnimationInfo> listAnimations(Identifier profileId) {
    return profileId != null ? List.copyOf(animationProvider.listAnimations(profileId)) : List.of();
  }

  public static List<String> listAnimationVariants(Identifier profileId, String baseName) {
    return profileId != null && baseName != null && !baseName.isEmpty()
        ? List.copyOf(animationProvider.listAnimationVariants(profileId, baseName))
        : List.of();
  }

  public static void setTextureProvider(TextureProvider provider) {
    textureProvider = provider != null ? provider : EMPTY_TEXTURE_PROVIDER;
  }

  public static List<String> listTextureSlots(Identifier profileId) {
    return profileId != null ? List.copyOf(textureProvider.listTextureSlots(profileId)) : List.of();
  }

  public static List<Identifier> listTextureVariants(Identifier profileId, String slot) {
    return profileId != null && slot != null && !slot.isEmpty()
        ? List.copyOf(textureProvider.listTextureVariants(profileId, slot))
        : List.of();
  }

  public static Identifier getProfileId(String entityModel) {
    if (entityModel != null && !entityModel.isEmpty()) {
      Identifier profileId = Identifier.tryParse(entityModel);
      if (profileId != null) {
        return profileId;
      }
    }
    return Identifier.parse(DEFAULT_PROFILE);
  }

  public static ModelType getModelType(String bodyTypeName) {
    if (bodyTypeName == null || bodyTypeName.isEmpty()) {
      return ModelType.HUMANOID;
    }
    return switch (bodyTypeName.toUpperCase(Locale.ROOT)) {
      case "QUADRUPED", "AMPHIBIOUS", "AQUATIC" -> ModelType.QUADRUPED;
      case "WINGED" -> ModelType.AVIAN;
      case "WINGED_HUMANOID" -> ModelType.PIXIE;
      case "ARTHROPOD" -> ModelType.SPIDER;
      case "STATIC", "CUBOID", "FLOATING" -> ModelType.SLIME;
      default -> ModelType.HUMANOID;
    };
  }

  public static ModelPartType getModelPartType(String partName) {
    if (partName == null || partName.isEmpty()) {
      return ModelPartType.UNKNOWN;
    }
    return switch (partName.toLowerCase(Locale.ROOT)) {
      case "front_left_leg" -> ModelPartType.LEFT_FRONT_LEG;
      case "front_right_leg" -> ModelPartType.RIGHT_FRONT_LEG;
      case "back_left_leg" -> ModelPartType.LEFT_HIND_LEG;
      case "back_right_leg" -> ModelPartType.RIGHT_HIND_LEG;
      case "tail_fin" -> ModelPartType.TAIL2;
      default -> ModelPartType.get(partName);
    };
  }

  public interface AnimationProvider {
    List<ModelAnimationInfo> listAnimations(Identifier profileId);

    List<String> listAnimationVariants(Identifier profileId, String baseName);
  }

  public interface TextureProvider {
    List<String> listTextureSlots(Identifier profileId);

    List<Identifier> listTextureVariants(Identifier profileId, String slot);
  }

  public record ProfileDimensions(float width, float height, float eyeHeight) {}
}
