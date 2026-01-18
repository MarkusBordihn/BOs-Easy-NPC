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

package de.markusbordihn.easynpc.data.preset;

import com.mojang.serialization.Codec;
import com.mojang.serialization.codecs.RecordCodecBuilder;
import net.minecraft.nbt.CompoundTag;
import net.minecraft.network.RegistryFriendlyByteBuf;
import net.minecraft.network.codec.StreamCodec;

public record PresetMetadata(
    String name,
    String category,
    String version,
    String author,
    long created,
    long modified,
    String description,
    String entityTypeId,
    String variantType) {

  public static final String TAG_NAME = "name";
  public static final String TAG_CATEGORY = "category";
  public static final String TAG_VERSION = "version";
  public static final String TAG_AUTHOR = "author";
  public static final String TAG_CREATED = "created";
  public static final String TAG_MODIFIED = "modified";
  public static final String TAG_DESCRIPTION = "description";
  public static final String TAG_ENTITY_TYPE_ID = "entityTypeId";
  public static final String TAG_VARIANT_TYPE = "variantType";

  public static final String DEFAULT_NAME = "Unnamed Preset";
  public static final String DEFAULT_CATEGORY = "Custom";
  public static final String DEFAULT_VERSION = "1.0.0";
  public static final String DEFAULT_AUTHOR = "Unknown";
  public static final String DEFAULT_DESCRIPTION = "";

  public static final Codec<PresetMetadata> CODEC =
      RecordCodecBuilder.create(
          instance ->
              instance
                  .group(
                      Codec.STRING
                          .optionalFieldOf(TAG_NAME, DEFAULT_NAME)
                          .forGetter(PresetMetadata::name),
                      Codec.STRING
                          .optionalFieldOf(TAG_CATEGORY, DEFAULT_CATEGORY)
                          .forGetter(PresetMetadata::category),
                      Codec.STRING
                          .optionalFieldOf(TAG_VERSION, DEFAULT_VERSION)
                          .forGetter(PresetMetadata::version),
                      Codec.STRING
                          .optionalFieldOf(TAG_AUTHOR, DEFAULT_AUTHOR)
                          .forGetter(PresetMetadata::author),
                      Codec.LONG
                          .optionalFieldOf(TAG_CREATED, 0L)
                          .forGetter(PresetMetadata::created),
                      Codec.LONG
                          .optionalFieldOf(TAG_MODIFIED, 0L)
                          .forGetter(PresetMetadata::modified),
                      Codec.STRING
                          .optionalFieldOf(TAG_DESCRIPTION, DEFAULT_DESCRIPTION)
                          .forGetter(PresetMetadata::description),
                      Codec.STRING
                          .optionalFieldOf(TAG_ENTITY_TYPE_ID, "")
                          .forGetter(
                              metadata ->
                                  metadata.entityTypeId() != null ? metadata.entityTypeId() : ""),
                      Codec.STRING
                          .optionalFieldOf(TAG_VARIANT_TYPE, "")
                          .forGetter(
                              metadata ->
                                  metadata.variantType() != null ? metadata.variantType() : ""))
                  .apply(
                      instance,
                      (name,
                          category,
                          version,
                          author,
                          created,
                          modified,
                          description,
                          entityTypeId,
                          variantType) ->
                          new PresetMetadata(
                              name,
                              category,
                              version,
                              author,
                              created,
                              modified,
                              description,
                              entityTypeId.isEmpty() ? null : entityTypeId,
                              variantType.isEmpty() ? null : variantType)));

  public static final StreamCodec<RegistryFriendlyByteBuf, PresetMetadata> STREAM_CODEC =
      new StreamCodec<>() {
        @Override
        public PresetMetadata decode(RegistryFriendlyByteBuf buffer) {
          String name = buffer.readUtf();
          String category = buffer.readUtf();
          String version = buffer.readUtf();
          String author = buffer.readUtf();
          long created = buffer.readVarLong();
          long modified = buffer.readVarLong();
          String description = buffer.readUtf();
          String entityTypeId = buffer.readUtf();
          String variantType = buffer.readUtf();
          return new PresetMetadata(
              name,
              category,
              version,
              author,
              created,
              modified,
              description,
              entityTypeId.isEmpty() ? null : entityTypeId,
              variantType.isEmpty() ? null : variantType);
        }

        @Override
        public void encode(RegistryFriendlyByteBuf buffer, PresetMetadata metadata) {
          buffer.writeUtf(metadata.name());
          buffer.writeUtf(metadata.category());
          buffer.writeUtf(metadata.version());
          buffer.writeUtf(metadata.author());
          buffer.writeVarLong(metadata.created());
          buffer.writeVarLong(metadata.modified());
          buffer.writeUtf(metadata.description());
          buffer.writeUtf(metadata.entityTypeId() != null ? metadata.entityTypeId() : "");
          buffer.writeUtf(metadata.variantType() != null ? metadata.variantType() : "");
        }
      };

  public PresetMetadata {
    if (name == null || name.isEmpty()) {
      name = DEFAULT_NAME;
    }
    if (category == null || category.isEmpty()) {
      category = DEFAULT_CATEGORY;
    }
    if (version == null || version.isEmpty()) {
      version = DEFAULT_VERSION;
    }
    if (author == null || author.isEmpty()) {
      author = DEFAULT_AUTHOR;
    }
    if (created <= 0) {
      created = System.currentTimeMillis();
    }
    if (modified <= 0) {
      modified = System.currentTimeMillis();
    }
    if (description == null) {
      description = DEFAULT_DESCRIPTION;
    }
    // entityTypeId and variantType are optional, can be null
  }

  public static PresetMetadata createDefault() {
    return new PresetMetadata(
        DEFAULT_NAME,
        DEFAULT_CATEGORY,
        DEFAULT_VERSION,
        DEFAULT_AUTHOR,
        System.currentTimeMillis(),
        System.currentTimeMillis(),
        DEFAULT_DESCRIPTION,
        null,
        null);
  }

  public static PresetMetadata getDefault() {
    return createDefault();
  }

  public static PresetMetadata createDefault(String name, String author) {
    return new PresetMetadata(
        name,
        DEFAULT_CATEGORY,
        DEFAULT_VERSION,
        author,
        System.currentTimeMillis(),
        System.currentTimeMillis(),
        DEFAULT_DESCRIPTION,
        null,
        null);
  }

  public static PresetMetadata fromCompoundTag(CompoundTag tag) {
    if (tag == null || tag.isEmpty()) {
      return createDefault();
    }

    return new PresetMetadata(
        tag.contains(TAG_NAME) ? tag.getString(TAG_NAME) : DEFAULT_NAME,
        tag.contains(TAG_CATEGORY) ? tag.getString(TAG_CATEGORY) : DEFAULT_CATEGORY,
        tag.contains(TAG_VERSION) ? tag.getString(TAG_VERSION) : DEFAULT_VERSION,
        tag.contains(TAG_AUTHOR) ? tag.getString(TAG_AUTHOR) : DEFAULT_AUTHOR,
        tag.contains(TAG_CREATED) ? tag.getLong(TAG_CREATED) : System.currentTimeMillis(),
        tag.contains(TAG_MODIFIED) ? tag.getLong(TAG_MODIFIED) : System.currentTimeMillis(),
        tag.contains(TAG_DESCRIPTION) ? tag.getString(TAG_DESCRIPTION) : DEFAULT_DESCRIPTION,
        tag.contains(TAG_ENTITY_TYPE_ID) ? tag.getString(TAG_ENTITY_TYPE_ID) : null,
        tag.contains(TAG_VARIANT_TYPE) ? tag.getString(TAG_VARIANT_TYPE) : null);
  }

  public static PresetMetadata fromPresetData(CompoundTag presetData) {
    if (presetData == null
        || !presetData.contains(
            de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable.PRESET_METADATA_TAG)) {
      return createDefault();
    }
    PresetMetadata metadata =
        fromCompoundTag(
            presetData.getCompound(
                de.markusbordihn.easynpc.entity.easynpc.data.PresetDataCapable
                    .PRESET_METADATA_TAG));

    // If metadata doesn't have preview data, try to extract from preset data
    if (metadata.entityTypeId() == null || metadata.variantType() == null) {
      String entityTypeId = metadata.entityTypeId();
      String variantType = metadata.variantType();

      if (entityTypeId == null && presetData.contains("id")) {
        entityTypeId = presetData.getString("id");
      }

      if (variantType == null && presetData.contains("VariantType")) {
        variantType = presetData.getString("VariantType");
      }

      if (entityTypeId != null || variantType != null) {
        metadata = metadata.withPreviewData(entityTypeId, variantType);
      }
    }

    return metadata;
  }

  public CompoundTag toCompoundTag() {
    CompoundTag tag = new CompoundTag();
    tag.putString(TAG_NAME, name);
    tag.putString(TAG_CATEGORY, category);
    tag.putString(TAG_VERSION, version);
    tag.putString(TAG_AUTHOR, author);
    tag.putLong(TAG_CREATED, created);
    tag.putLong(TAG_MODIFIED, modified);
    tag.putString(TAG_DESCRIPTION, description);

    if (entityTypeId != null && !entityTypeId.isEmpty()) {
      tag.putString(TAG_ENTITY_TYPE_ID, entityTypeId);
    }
    if (variantType != null && !variantType.isEmpty()) {
      tag.putString(TAG_VARIANT_TYPE, variantType);
    }

    return tag;
  }

  public PresetMetadata withModifiedTime(long modifiedTime) {
    return new PresetMetadata(
        name,
        category,
        version,
        author,
        created,
        modifiedTime,
        description,
        entityTypeId,
        variantType);
  }

  public PresetMetadata withCurrentModifiedTime() {
    return withModifiedTime(System.currentTimeMillis());
  }

  public PresetMetadata withName(String newName) {
    return new PresetMetadata(
        newName,
        category,
        version,
        author,
        created,
        System.currentTimeMillis(),
        description,
        entityTypeId,
        variantType);
  }

  public PresetMetadata withCategory(String newCategory) {
    return new PresetMetadata(
        name,
        newCategory,
        version,
        author,
        created,
        System.currentTimeMillis(),
        description,
        entityTypeId,
        variantType);
  }

  public PresetMetadata withVersion(String newVersion) {
    return new PresetMetadata(
        name,
        category,
        newVersion,
        author,
        created,
        System.currentTimeMillis(),
        description,
        entityTypeId,
        variantType);
  }

  public PresetMetadata withAuthor(String newAuthor) {
    return new PresetMetadata(
        name,
        category,
        version,
        newAuthor,
        created,
        System.currentTimeMillis(),
        description,
        entityTypeId,
        variantType);
  }

  public PresetMetadata withDescription(String newDescription) {
    return new PresetMetadata(
        name,
        category,
        version,
        author,
        created,
        System.currentTimeMillis(),
        newDescription,
        entityTypeId,
        variantType);
  }

  public PresetMetadata withPreviewData(String newEntityTypeId, String newVariantType) {
    return new PresetMetadata(
        name,
        category,
        version,
        author,
        created,
        modified,
        description,
        newEntityTypeId,
        newVariantType);
  }
}
